{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Bijection Management for the Autoformalizer Protocol
--
-- This module manages the correspondence (bijection) between source document
-- elements and their formal Agda counterparts. The bijection tracks:
--   * Which source sections have been formalized
--   * Which formal definitions correspond to which source content
--   * Coverage statistics for the formalization effort
--
-- Operations:
--   * source_for_hole: Find source reference corresponding to a hole
--   * formal_for_section: Find formal reference for a source section
--   * get_coverage: Compute formalization coverage statistics
--   * update_bijection: Add a new source-formal correspondence

module AgdaMCP.Autoformalizer.Bijection
    ( -- * Bijection State
      BijectionState(..)
    , emptyBijection
    , BijectionEntry(..)

      -- * Bijection Operations
    , sourceForHole
    , formalForSection
    , getCoverage
    , updateBijection

      -- * Query Operations
    , lookupBySource
    , lookupByFormal
    , allEntries
    , entriesForModule
    , entriesForSection

      -- * Statistics
    , countCoveredSections
    , listUncoveredSections
    , listHolesWithoutSource

      -- * Persistence
    , saveBijection
    , loadBijection
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:))
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BL
import Control.Exception (try, SomeException)
import GHC.Generics (Generic)

import AgdaMCP.Autoformalizer.Types

--------------------------------------------------------------------------------
-- Bijection State
--------------------------------------------------------------------------------

-- | A single entry in the bijection
data BijectionEntry = BijectionEntry
    { entrySource :: SourceRef           -- ^ Reference in source document
    , entryFormal :: FormalRef           -- ^ Reference in Agda formalization
    , entryConfidence :: Double          -- ^ Confidence score (0-1)
    , entryNotes :: Maybe Text           -- ^ Optional notes
    , entryHoles :: [HoleId]             -- ^ Associated unfilled holes
    }
    deriving (Show, Eq, Generic)

instance ToJSON BijectionEntry where
    toJSON BijectionEntry{..} =
        JSON.object
            [ "source" .= entrySource
            , "formal" .= entryFormal
            , "confidence" .= entryConfidence
            , "notes" .= entryNotes
            , "holes" .= entryHoles
            ]

instance FromJSON BijectionEntry where
    parseJSON = JSON.withObject "BijectionEntry" $ \v ->
        BijectionEntry
            <$> v .: "source"
            <*> v .: "formal"
            <*> v .: "confidence"
            <*> v .: "notes"
            <*> v .: "holes"

-- | State of the bijection between source and formal elements
data BijectionState = BijectionState
    { -- | Primary index: source ref -> entry
      bijBySource :: Map SourceRef BijectionEntry
      -- | Secondary index: formal ref -> entry
    , bijByFormal :: Map FormalRef BijectionEntry
      -- | Index: section -> entries in that section
    , bijBySection :: Map SectionId [BijectionEntry]
      -- | Index: module -> entries in that module
    , bijByModule :: Map ModulePath [BijectionEntry]
      -- | Index: hole -> source ref (for quick lookup)
    , bijHoleToSource :: Map HoleId SourceRef
      -- | All known source sections (for coverage calculation)
    , bijAllSections :: Set SectionId
      -- | All known holes in target
    , bijAllHoles :: Set HoleId
      -- | All known postulates in target
    , bijAllPostulates :: Set Name
    }
    deriving (Show, Eq)

-- | Empty bijection state
emptyBijection :: BijectionState
emptyBijection = BijectionState
    { bijBySource = Map.empty
    , bijByFormal = Map.empty
    , bijBySection = Map.empty
    , bijByModule = Map.empty
    , bijHoleToSource = Map.empty
    , bijAllSections = Set.empty
    , bijAllHoles = Set.empty
    , bijAllPostulates = Set.empty
    }

--------------------------------------------------------------------------------
-- Core Bijection Operations
--------------------------------------------------------------------------------

-- | Find the source reference corresponding to a hole
--
-- Precondition: True
-- Postcondition:
--   Just ref -> (ref, _) ∈ Bijection ∧ _.name corresponds to Input
--   Nothing -> ∄ (ref, formal) ∈ Bijection. formal corresponds to Input
sourceForHole :: BijectionState -> HoleId -> Maybe SourceRef
sourceForHole state hid = Map.lookup hid (bijHoleToSource state)

-- | Find the formal reference corresponding to a source section
--
-- Precondition: True
-- Postcondition:
--   Just ref -> (_, ref) ∈ Bijection ∧ _.section = Input
--   Nothing -> ∄ (source, ref) ∈ Bijection. source.section = Input
formalForSection :: BijectionState -> SectionId -> Maybe FormalRef
formalForSection state sid =
    case Map.lookup sid (bijBySection state) of
        Nothing -> Nothing
        Just [] -> Nothing
        Just (entry:_) -> Just (entryFormal entry)  -- Return first match

-- | Compute coverage statistics
--
-- Precondition: True
-- Postcondition:
--   Output.percent = |{s ∈ Source.sections | formal_for_section(s) ≠ Nothing}| / |Source.sections| × 100
--   Output.holesRemaining = |Target.allHoles|
--   Output.postulatesRemaining = |Target.postulates|
--   Output.uncoveredSections = {s ∈ Source.sections | formal_for_section(s) = Nothing}
getCoverage :: BijectionState -> Coverage
getCoverage state =
    let allSections = Set.toList (bijAllSections state)
        coveredSections = filter (isJust . formalForSection state) allSections
        totalSections = length allSections
        coveredCount = length coveredSections
        percent = if totalSections == 0
                  then 100  -- No sections means 100% covered
                  else (coveredCount * 100) `div` totalSections
        uncovered = filter (isNothing . formalForSection state) allSections
        holesRemaining = Set.size (bijAllHoles state)
        postulatesRemaining = Set.size (bijAllPostulates state)
    in Coverage
        { coveragePercent = percent
        , coverageHolesRemaining = holesRemaining
        , coveragePostulatesRemaining = postulatesRemaining
        , coverageUncoveredSections = uncovered
        }
  where
    isJust (Just _) = True
    isJust Nothing = False
    isNothing = not . isJust

-- | Add a new correspondence to the bijection
--
-- Precondition: True
-- Effect: Bijection := Bijection ∪ {Input}
-- Postcondition: Input ∈ Bijection
updateBijection :: BijectionState -> SourceRef -> FormalRef -> BijectionState
updateBijection state srcRef formalRef =
    let entry = BijectionEntry
            { entrySource = srcRef
            , entryFormal = formalRef
            , entryConfidence = 1.0  -- Default confidence
            , entryNotes = Nothing
            , entryHoles = []
            }
    in addEntry state entry

-- | Add an entry to the bijection state
addEntry :: BijectionState -> BijectionEntry -> BijectionState
addEntry state entry@BijectionEntry{..} =
    let srcRef = entrySource
        formalRef = entryFormal
        sid = sourceRefSection srcRef
        modPath = formalRefModule formalRef
    in state
        { bijBySource = Map.insert srcRef entry (bijBySource state)
        , bijByFormal = Map.insert formalRef entry (bijByFormal state)
        , bijBySection = Map.insertWith (++) sid [entry] (bijBySection state)
        , bijByModule = Map.insertWith (++) modPath [entry] (bijByModule state)
        , bijHoleToSource = foldr (\h m -> Map.insert h srcRef m)
                                  (bijHoleToSource state)
                                  entryHoles
        }

--------------------------------------------------------------------------------
-- Query Operations
--------------------------------------------------------------------------------

-- | Look up entry by source reference
lookupBySource :: BijectionState -> SourceRef -> Maybe BijectionEntry
lookupBySource state srcRef = Map.lookup srcRef (bijBySource state)

-- | Look up entry by formal reference
lookupByFormal :: BijectionState -> FormalRef -> Maybe BijectionEntry
lookupByFormal state formalRef = Map.lookup formalRef (bijByFormal state)

-- | Get all bijection entries
allEntries :: BijectionState -> [BijectionEntry]
allEntries state = Map.elems (bijBySource state)

-- | Get entries for a specific module
entriesForModule :: BijectionState -> ModulePath -> [BijectionEntry]
entriesForModule state modPath =
    fromMaybe [] (Map.lookup modPath (bijByModule state))

-- | Get entries for a specific section
entriesForSection :: BijectionState -> SectionId -> [BijectionEntry]
entriesForSection state sid =
    fromMaybe [] (Map.lookup sid (bijBySection state))

--------------------------------------------------------------------------------
-- Statistics
--------------------------------------------------------------------------------

-- | Count number of covered sections
countCoveredSections :: BijectionState -> Int
countCoveredSections state =
    Set.size $ Set.fromList
        [ sourceRefSection src
        | src <- Map.keys (bijBySource state)
        ]

-- | List sections without formal counterpart
listUncoveredSections :: BijectionState -> [SectionId]
listUncoveredSections state =
    let covered = Set.fromList
            [ sourceRefSection src
            | src <- Map.keys (bijBySource state)
            ]
    in Set.toList $ Set.difference (bijAllSections state) covered

-- | List holes without associated source reference
listHolesWithoutSource :: BijectionState -> [HoleId]
listHolesWithoutSource state =
    let holesWithSource = Set.fromList $ Map.keys (bijHoleToSource state)
    in Set.toList $ Set.difference (bijAllHoles state) holesWithSource

--------------------------------------------------------------------------------
-- Modification Operations
--------------------------------------------------------------------------------

-- | Associate a hole with a source reference
associateHoleWithSource :: BijectionState -> HoleId -> SourceRef -> BijectionState
associateHoleWithSource state hid srcRef =
    state { bijHoleToSource = Map.insert hid srcRef (bijHoleToSource state) }

-- | Remove a hole from the bijection (when filled)
removeHole :: BijectionState -> HoleId -> BijectionState
removeHole state hid =
    state
        { bijHoleToSource = Map.delete hid (bijHoleToSource state)
        , bijAllHoles = Set.delete hid (bijAllHoles state)
        }

-- | Register all known sections
registerSections :: BijectionState -> [SectionId] -> BijectionState
registerSections state sids =
    state { bijAllSections = Set.union (bijAllSections state) (Set.fromList sids) }

-- | Register all known holes
registerHoles :: BijectionState -> [HoleId] -> BijectionState
registerHoles state hids =
    state { bijAllHoles = Set.union (bijAllHoles state) (Set.fromList hids) }

-- | Register all known postulates
registerPostulates :: BijectionState -> [Name] -> BijectionState
registerPostulates state names =
    state { bijAllPostulates = Set.union (bijAllPostulates state) (Set.fromList names) }

-- | Remove a postulate from the bijection (when proved)
removePostulate :: BijectionState -> Name -> BijectionState
removePostulate state name =
    state { bijAllPostulates = Set.delete name (bijAllPostulates state) }

--------------------------------------------------------------------------------
-- Update Operations with Confidence
--------------------------------------------------------------------------------

-- | Update bijection with confidence score
updateBijectionWithConfidence :: BijectionState -> SourceRef -> FormalRef -> Double -> BijectionState
updateBijectionWithConfidence state srcRef formalRef confidence =
    let entry = BijectionEntry
            { entrySource = srcRef
            , entryFormal = formalRef
            , entryConfidence = confidence
            , entryNotes = Nothing
            , entryHoles = []
            }
    in addEntry state entry

-- | Update bijection with notes
updateBijectionWithNotes :: BijectionState -> SourceRef -> FormalRef -> Text -> BijectionState
updateBijectionWithNotes state srcRef formalRef notes =
    let entry = BijectionEntry
            { entrySource = srcRef
            , entryFormal = formalRef
            , entryConfidence = 1.0
            , entryNotes = Just notes
            , entryHoles = []
            }
    in addEntry state entry

-- | Update entry's associated holes
addHolesToEntry :: BijectionState -> SourceRef -> [HoleId] -> BijectionState
addHolesToEntry state srcRef hids =
    case Map.lookup srcRef (bijBySource state) of
        Nothing -> state  -- Entry doesn't exist
        Just entry ->
            let updatedEntry = entry { entryHoles = entryHoles entry ++ hids }
                newHoleToSource = foldr (\h m -> Map.insert h srcRef m)
                                        (bijHoleToSource state)
                                        hids
            in state
                { bijBySource = Map.insert srcRef updatedEntry (bijBySource state)
                , bijByFormal = Map.insert (entryFormal entry) updatedEntry (bijByFormal state)
                , bijHoleToSource = newHoleToSource
                }

--------------------------------------------------------------------------------
-- Persistence
--------------------------------------------------------------------------------

-- | Bijection file format for persistence
data BijectionFile = BijectionFile
    { bfEntries :: [BijectionEntry]
    , bfSections :: [SectionId]
    , bfHoles :: [HoleId]
    , bfPostulates :: [Name]
    }
    deriving (Show, Eq, Generic)

instance ToJSON BijectionFile where
    toJSON BijectionFile{..} =
        JSON.object
            [ "entries" .= bfEntries
            , "sections" .= bfSections
            , "holes" .= bfHoles
            , "postulates" .= bfPostulates
            ]

instance FromJSON BijectionFile where
    parseJSON = JSON.withObject "BijectionFile" $ \v ->
        BijectionFile
            <$> v .: "entries"
            <*> v .: "sections"
            <*> v .: "holes"
            <*> v .: "postulates"

-- | Save bijection state to a file
saveBijection :: FilePath -> BijectionState -> IO (Either Text ())
saveBijection path state = do
    let file = BijectionFile
            { bfEntries = allEntries state
            , bfSections = Set.toList (bijAllSections state)
            , bfHoles = Set.toList (bijAllHoles state)
            , bfPostulates = Set.toList (bijAllPostulates state)
            }
        json = JSON.encode file
    result <- try (BL.writeFile path json) :: IO (Either SomeException ())
    case result of
        Left err -> return $ Left $ "Failed to save bijection: " <> T.pack (show err)
        Right () -> return $ Right ()

-- | Load bijection state from a file
loadBijection :: FilePath -> IO (Either Text BijectionState)
loadBijection path = do
    result <- try (BL.readFile path) :: IO (Either SomeException BL.ByteString)
    case result of
        Left err -> return $ Left $ "Failed to read bijection file: " <> T.pack (show err)
        Right bytes ->
            case JSON.decode bytes of
                Nothing -> return $ Left "Failed to parse bijection file as JSON"
                Just file -> return $ Right $ reconstructState file

-- | Reconstruct bijection state from file format
reconstructState :: BijectionFile -> BijectionState
reconstructState BijectionFile{..} =
    let baseState = emptyBijection
            { bijAllSections = Set.fromList bfSections
            , bijAllHoles = Set.fromList bfHoles
            , bijAllPostulates = Set.fromList bfPostulates
            }
    in foldr (flip addEntry) baseState bfEntries
