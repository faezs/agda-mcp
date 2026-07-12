{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Types for the Autoformalizer REPL Protocol
--
-- This module implements the type definitions from the Autoformalizer REPL
-- Protocol Specification v1.0. The protocol enables session-typed interaction
-- with an autoformalizer that completes partial Agda formalizations guided
-- by source documents.
--
-- The system has three components:
--   * Source: A document being formalized (read-only)
--   * Target: An Agda project with holes and postulates (read-write via agda-mcp)
--   * Bijection: Correspondence between source elements and formal elements

module AgdaMCP.Autoformalizer.Types
    ( -- * Identifiers
      SectionId(..)
    , TheoremId(..)
    , ModulePath(..)
    , HoleId(..)
    , Name(..)
    , Term(..)
    , Var(..)
    , Pattern(..)

      -- * Source Domain
    , SourceContent(..)
    , MathExpr(..)
    , TheoremContent(..)
    , Match(..)

      -- * Target Domain
    , LoadResult(..)
    , Goal(..)
    , GoalContext(..)
    , GiveResult(..)
    , RefineResult(..)
    , CaseSplitResult(..)
    , AutoResult(..)

      -- * Bijection Domain
    , SourceRef(..)
    , FormalRef(..)
    , Coverage(..)

      -- * Task and Result
    , Task(..)
    , Focus(..)
    , Result(..)
    , Status(..)
    , Effect(..)
    , Output(..)

      -- * Priority
    , Priority(..)
    , succPriority
    , priorityToInt
    , priorityFromInt

      -- * Session Operations
    , SourceOp(..)
    , TargetOp(..)
    , BijectionOp(..)
    , SessionOp(..)
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), (.:?))
import Data.Aeson.Types (Parser)
import qualified Data.Aeson as JSON
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- Identifiers
--------------------------------------------------------------------------------

-- | Source document section identifier
newtype SectionId = SectionId { unSectionId :: Text }
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (ToJSON, FromJSON)

-- | Source theorem/definition identifier
newtype TheoremId = TheoremId { unTheoremId :: Text }
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (ToJSON, FromJSON)

-- | Agda module path (e.g., "Neural.Homotopy.GammaSpaces")
newtype ModulePath = ModulePath { unModulePath :: Text }
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (ToJSON, FromJSON)

-- | Agda hole identifier (e.g., "?0" or numeric 0)
newtype HoleId = HoleId { unHoleId :: Int }
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (ToJSON, FromJSON)

-- | Agda name
newtype Name = Name { unName :: Text }
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (ToJSON, FromJSON)

-- | Agda term (concrete syntax)
newtype Term = Term { unTerm :: Text }
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (ToJSON, FromJSON)

-- | Variable name for case split
newtype Var = Var { unVar :: Text }
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (ToJSON, FromJSON)

-- | Search pattern for grep_source
newtype Pattern = Pattern { unPattern :: Text }
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Source Domain
--------------------------------------------------------------------------------

-- | Content of a source document section
data SourceContent = SourceContent
    { sourceText :: Text           -- ^ Raw text content
    , sourceMath :: [MathExpr]     -- ^ Extracted math expressions
    }
    deriving (Show, Eq, Generic)

instance ToJSON SourceContent where
    toJSON (SourceContent txt math) =
        JSON.object ["text" .= txt, "math" .= math]

instance FromJSON SourceContent where
    parseJSON = JSON.withObject "SourceContent" $ \v ->
        SourceContent <$> v .: "text" <*> v .: "math"

-- | A mathematical expression within source content
data MathExpr = MathExpr
    { mathLatex :: Text            -- ^ LaTeX representation
    , mathLocation :: (Int, Int)   -- ^ Start and end position
    }
    deriving (Show, Eq, Generic)

instance ToJSON MathExpr where
    toJSON (MathExpr latex (start, end)) =
        JSON.object ["latex" .= latex, "location" .= [start, end]]

instance FromJSON MathExpr where
    parseJSON = JSON.withObject "MathExpr" $ \v -> do
        latex <- v .: "latex"
        loc <- v .: "location"
        case loc of
            [start, end] -> pure $ MathExpr latex (start, end)
            _ -> fail "Expected [start, end] for location"

-- | Content of a theorem from the source document
data TheoremContent = TheoremContent
    { theoremStatement :: Text          -- ^ The theorem statement
    , theoremProof :: Maybe Text        -- ^ Optional proof text
    }
    deriving (Show, Eq, Generic)

instance ToJSON TheoremContent where
    toJSON (TheoremContent stmt proof) =
        JSON.object ["statement" .= stmt, "proof" .= proof]

instance FromJSON TheoremContent where
    parseJSON = JSON.withObject "TheoremContent" $ \v ->
        TheoremContent <$> v .: "statement" <*> v .:? "proof"

-- | A match from grep_source
data Match = Match
    { matchSection :: SectionId        -- ^ Section containing the match
    , matchRange :: (Int, Int)         -- ^ Character range of match
    , matchSnippet :: Text             -- ^ Snippet of matched text
    }
    deriving (Show, Eq, Generic)

instance ToJSON Match where
    toJSON (Match section (start, end) snippet) =
        JSON.object
            [ "section" .= section
            , "range" .= [start, end]
            , "snippet" .= snippet
            ]

instance FromJSON Match where
    parseJSON = JSON.withObject "Match" $ \v -> do
        section <- v .: "section"
        range <- v .: "range"
        snippet <- v .: "snippet"
        case range of
            [start, end] -> pure $ Match section (start, end) snippet
            _ -> fail "Expected [start, end] for range"

--------------------------------------------------------------------------------
-- Target Domain (Agda interaction via agda-mcp)
--------------------------------------------------------------------------------

-- | Result of loading an Agda module
data LoadResult
    = LoadSuccess { loadGoals :: [Goal] }
    | LoadError { loadErrorMessage :: Text }
    deriving (Show, Eq, Generic)

instance ToJSON LoadResult where
    toJSON (LoadSuccess goals) =
        JSON.object ["tag" .= ("LoadSuccess" :: Text), "goals" .= goals]
    toJSON (LoadError msg) =
        JSON.object ["tag" .= ("LoadError" :: Text), "message" .= msg]

instance FromJSON LoadResult where
    parseJSON = JSON.withObject "LoadResult" $ \v -> do
        tag <- v .: "tag" :: Parser Text
        case tag of
            "LoadSuccess" -> LoadSuccess <$> v .: "goals"
            "LoadError" -> LoadError <$> v .: "message"
            _ -> fail $ "Unknown LoadResult tag: " <> T.unpack tag

-- | An Agda goal/hole
data Goal = Goal
    { goalId :: HoleId                      -- ^ Hole identifier
    , goalType :: Text                      -- ^ Expected type
    , goalContext :: [(Name, Text)]         -- ^ Local context (name, type)
    }
    deriving (Show, Eq, Generic)

instance ToJSON Goal where
    toJSON (Goal gid gtype ctx) =
        JSON.object
            [ "id" .= gid
            , "type" .= gtype
            , "context" .= map (\(n, t) -> JSON.object ["name" .= n, "type" .= t]) ctx
            ]

instance FromJSON Goal where
    parseJSON = JSON.withObject "Goal" $ \v -> do
        gid <- v .: "id"
        gtype <- v .: "type"
        ctxRaw <- v .: "context"
        ctx <- mapM parseCtxEntry ctxRaw
        pure $ Goal gid gtype ctx
      where
        parseCtxEntry = JSON.withObject "context entry" $ \e ->
            (,) <$> e .: "name" <*> e .: "type"

-- | Extended goal context with available lemmas
data GoalContext = GoalContext
    { gcGoal :: Goal                        -- ^ The goal itself
    , gcLocalNames :: [Name]                -- ^ Names in scope
    , gcAvailableLemmas :: [Name]           -- ^ Relevant lemmas
    }
    deriving (Show, Eq, Generic)

instance ToJSON GoalContext where
    toJSON (GoalContext goal locals lemmas) =
        JSON.object
            [ "goal" .= goal
            , "localNames" .= locals
            , "availableLemmas" .= lemmas
            ]

instance FromJSON GoalContext where
    parseJSON = JSON.withObject "GoalContext" $ \v ->
        GoalContext <$> v .: "goal" <*> v .: "localNames" <*> v .: "availableLemmas"

-- | Result of giving a term to fill a hole
data GiveResult
    = GiveSuccess                           -- ^ Hole filled successfully
    | GiveRefined { giveNewHoles :: [HoleId] }  -- ^ Filled but created new holes
    | GiveError { giveErrorMessage :: Text }    -- ^ Failed to fill hole
    deriving (Show, Eq, Generic)

instance ToJSON GiveResult where
    toJSON GiveSuccess =
        JSON.object ["tag" .= ("GiveSuccess" :: Text)]
    toJSON (GiveRefined holes) =
        JSON.object ["tag" .= ("GiveRefined" :: Text), "newHoles" .= holes]
    toJSON (GiveError msg) =
        JSON.object ["tag" .= ("GiveError" :: Text), "message" .= msg]

instance FromJSON GiveResult where
    parseJSON = JSON.withObject "GiveResult" $ \v -> do
        tag <- v .: "tag" :: Parser Text
        case tag of
            "GiveSuccess" -> pure GiveSuccess
            "GiveRefined" -> GiveRefined <$> v .: "newHoles"
            "GiveError" -> GiveError <$> v .: "message"
            _ -> fail $ "Unknown GiveResult tag: " <> T.unpack tag

-- | Result of refining a goal
data RefineResult
    = RefineSuccess { refineNewHoles :: [HoleId] }
    | RefineError { refineErrorMessage :: Text }
    deriving (Show, Eq, Generic)

instance ToJSON RefineResult where
    toJSON (RefineSuccess holes) =
        JSON.object ["tag" .= ("RefineSuccess" :: Text), "newHoles" .= holes]
    toJSON (RefineError msg) =
        JSON.object ["tag" .= ("RefineError" :: Text), "message" .= msg]

instance FromJSON RefineResult where
    parseJSON = JSON.withObject "RefineResult" $ \v -> do
        tag <- v .: "tag" :: Parser Text
        case tag of
            "RefineSuccess" -> RefineSuccess <$> v .: "newHoles"
            "RefineError" -> RefineError <$> v .: "message"
            _ -> fail $ "Unknown RefineResult tag: " <> T.unpack tag

-- | Result of case splitting
data CaseSplitResult
    = SplitSuccess { splitNewHoles :: [HoleId] }
    | SplitError { splitErrorMessage :: Text }
    deriving (Show, Eq, Generic)

instance ToJSON CaseSplitResult where
    toJSON (SplitSuccess holes) =
        JSON.object ["tag" .= ("SplitSuccess" :: Text), "newHoles" .= holes]
    toJSON (SplitError msg) =
        JSON.object ["tag" .= ("SplitError" :: Text), "message" .= msg]

instance FromJSON CaseSplitResult where
    parseJSON = JSON.withObject "CaseSplitResult" $ \v -> do
        tag <- v .: "tag" :: Parser Text
        case tag of
            "SplitSuccess" -> SplitSuccess <$> v .: "newHoles"
            "SplitError" -> SplitError <$> v .: "message"
            _ -> fail $ "Unknown CaseSplitResult tag: " <> T.unpack tag

-- | Result of automatic proof search
data AutoResult
    = AutoSuccess { autoTerm :: Term }
    | AutoFailed
    deriving (Show, Eq, Generic)

instance ToJSON AutoResult where
    toJSON (AutoSuccess term) =
        JSON.object ["tag" .= ("AutoSuccess" :: Text), "term" .= term]
    toJSON AutoFailed =
        JSON.object ["tag" .= ("AutoFailed" :: Text)]

instance FromJSON AutoResult where
    parseJSON = JSON.withObject "AutoResult" $ \v -> do
        tag <- v .: "tag" :: Parser Text
        case tag of
            "AutoSuccess" -> AutoSuccess <$> v .: "term"
            "AutoFailed" -> pure AutoFailed
            _ -> fail $ "Unknown AutoResult tag: " <> T.unpack tag

--------------------------------------------------------------------------------
-- Bijection Domain
--------------------------------------------------------------------------------

-- | Reference to a location in the source document
data SourceRef = SourceRef
    { sourceRefSection :: SectionId    -- ^ Section containing the reference
    , sourceRefRange :: (Int, Int)     -- ^ Character range
    }
    deriving (Show, Eq, Ord, Generic)

instance ToJSON SourceRef where
    toJSON (SourceRef section (start, end)) =
        JSON.object ["section" .= section, "range" .= [start, end]]

instance FromJSON SourceRef where
    parseJSON = JSON.withObject "SourceRef" $ \v -> do
        section <- v .: "section"
        range <- v .: "range"
        case range of
            [start, end] -> pure $ SourceRef section (start, end)
            _ -> fail "Expected [start, end] for range"

-- | Reference to a formal element in Agda
data FormalRef = FormalRef
    { formalRefModule :: ModulePath    -- ^ Module containing the definition
    , formalRefName :: Name            -- ^ Name of the definition
    }
    deriving (Show, Eq, Ord, Generic)

instance ToJSON FormalRef where
    toJSON (FormalRef modPath name) =
        JSON.object ["module" .= modPath, "name" .= name]

instance FromJSON FormalRef where
    parseJSON = JSON.withObject "FormalRef" $ \v ->
        FormalRef <$> v .: "module" <*> v .: "name"

-- | Formalization coverage statistics
data Coverage = Coverage
    { coveragePercent :: Int               -- ^ 0-100 percentage covered
    , coverageHolesRemaining :: Int        -- ^ Number of unfilled holes
    , coveragePostulatesRemaining :: Int   -- ^ Number of postulates
    , coverageUncoveredSections :: [SectionId]  -- ^ Sections without formal counterpart
    }
    deriving (Show, Eq, Generic)

instance ToJSON Coverage where
    toJSON (Coverage pct holes posts uncovered) =
        JSON.object
            [ "percent" .= pct
            , "holesRemaining" .= holes
            , "postulatesRemaining" .= posts
            , "uncoveredSections" .= uncovered
            ]

instance FromJSON Coverage where
    parseJSON = JSON.withObject "Coverage" $ \v ->
        Coverage
            <$> v .: "percent"
            <*> v .: "holesRemaining"
            <*> v .: "postulatesRemaining"
            <*> v .: "uncoveredSections"

--------------------------------------------------------------------------------
-- Task and Result
--------------------------------------------------------------------------------

-- | The focus of a formalization task
data Focus
    = FillHole { focusHoleId :: HoleId }
    | FormalizeSection { focusSectionId :: SectionId }
    | ProvePostulate { focusPostulateName :: Name }
    deriving (Show, Eq, Generic)

instance ToJSON Focus where
    toJSON (FillHole hid) =
        JSON.object ["tag" .= ("FillHole" :: Text), "holeId" .= hid]
    toJSON (FormalizeSection sid) =
        JSON.object ["tag" .= ("FormalizeSection" :: Text), "sectionId" .= sid]
    toJSON (ProvePostulate name) =
        JSON.object ["tag" .= ("ProvePostulate" :: Text), "name" .= name]

instance FromJSON Focus where
    parseJSON = JSON.withObject "Focus" $ \v -> do
        tag <- v .: "tag" :: Parser Text
        case tag of
            "FillHole" -> FillHole <$> v .: "holeId"
            "FormalizeSection" -> FormalizeSection <$> v .: "sectionId"
            "ProvePostulate" -> ProvePostulate <$> v .: "name"
            _ -> fail $ "Unknown Focus tag: " <> T.unpack tag

-- | A task to be performed by the autoformalizer
data Task = Task
    { taskFocus :: Focus                                   -- ^ What to accomplish
    , taskSourceSlice :: Maybe (SectionId, SourceContent)  -- ^ Relevant source
    , taskTargetModule :: Maybe ModulePath                 -- ^ Target module
    , taskRelevantBijection :: [(SourceRef, FormalRef)]    -- ^ Known correspondences
    }
    deriving (Show, Eq, Generic)

instance ToJSON Task where
    toJSON (Task focus srcSlice targetMod bij) =
        JSON.object
            [ "focus" .= focus
            , "sourceSlice" .= fmap (\(s, c) -> JSON.object ["section" .= s, "content" .= c]) srcSlice
            , "targetModule" .= targetMod
            , "relevantBijection" .= map (\(s, f) -> JSON.object ["source" .= s, "formal" .= f]) bij
            ]

instance FromJSON Task where
    parseJSON = JSON.withObject "Task" $ \v -> do
        focus <- v .: "focus"
        srcSlice <- v .:? "sourceSlice" >>= \case
            Nothing -> pure Nothing
            Just o -> JSON.withObject "sourceSlice" (\s ->
                Just <$> ((,) <$> s .: "section" <*> s .: "content")) o
        targetMod <- v .:? "targetModule"
        bijRaw <- v .: "relevantBijection"
        bij <- mapM parseBijEntry bijRaw
        pure $ Task focus srcSlice targetMod bij
      where
        parseBijEntry = JSON.withObject "bijection entry" $ \e ->
            (,) <$> e .: "source" <*> e .: "formal"

-- | Status of a task execution
data Status
    = Success
    | Partial { partialReason :: Text }
    | Failed { failedReason :: Text }
    deriving (Show, Eq, Generic)

instance ToJSON Status where
    toJSON Success = JSON.object ["tag" .= ("Success" :: Text)]
    toJSON (Partial reason) =
        JSON.object ["tag" .= ("Partial" :: Text), "reason" .= reason]
    toJSON (Failed reason) =
        JSON.object ["tag" .= ("Failed" :: Text), "reason" .= reason]

instance FromJSON Status where
    parseJSON = JSON.withObject "Status" $ \v -> do
        tag <- v .: "tag" :: Parser Text
        case tag of
            "Success" -> pure Success
            "Partial" -> Partial <$> v .: "reason"
            "Failed" -> Failed <$> v .: "reason"
            _ -> fail $ "Unknown Status tag: " <> T.unpack tag

-- | An effect produced by task execution
data Effect
    = HoleFilled { effectHoleId :: HoleId, effectTerm :: Term }
    | BijectionUpdated { effectSource :: SourceRef, effectFormal :: FormalRef }
    | ModuleModified { effectModule :: ModulePath }
    deriving (Show, Eq, Generic)

instance ToJSON Effect where
    toJSON (HoleFilled hid term) =
        JSON.object ["tag" .= ("HoleFilled" :: Text), "holeId" .= hid, "term" .= term]
    toJSON (BijectionUpdated src formal) =
        JSON.object ["tag" .= ("BijectionUpdated" :: Text), "source" .= src, "formal" .= formal]
    toJSON (ModuleModified modPath) =
        JSON.object ["tag" .= ("ModuleModified" :: Text), "module" .= modPath]

instance FromJSON Effect where
    parseJSON = JSON.withObject "Effect" $ \v -> do
        tag <- v .: "tag" :: Parser Text
        case tag of
            "HoleFilled" -> HoleFilled <$> v .: "holeId" <*> v .: "term"
            "BijectionUpdated" -> BijectionUpdated <$> v .: "source" <*> v .: "formal"
            "ModuleModified" -> ModuleModified <$> v .: "module"
            _ -> fail $ "Unknown Effect tag: " <> T.unpack tag

-- | Result of a task execution
data Result = Result
    { resultStatus :: Status
    , resultOutput :: Maybe Term
    , resultEffects :: [Effect]
    }
    deriving (Show, Eq, Generic)

instance ToJSON Result where
    toJSON (Result status output effects) =
        JSON.object
            [ "status" .= status
            , "output" .= output
            , "effects" .= effects
            ]

instance FromJSON Result where
    parseJSON = JSON.withObject "Result" $ \v ->
        Result <$> v .: "status" <*> v .:? "output" <*> v .: "effects"

-- | Final output of a session
data Output = Output
    { outputTerm :: Maybe Term
    , outputEffects :: [Effect]
    , outputStatus :: Status
    }
    deriving (Show, Eq, Generic)

instance ToJSON Output where
    toJSON (Output term effects status) =
        JSON.object
            [ "term" .= term
            , "effects" .= effects
            , "status" .= status
            ]

instance FromJSON Output where
    parseJSON = JSON.withObject "Output" $ \v ->
        Output <$> v .:? "term" <*> v .: "effects" <*> v .: "status"

--------------------------------------------------------------------------------
-- Priority (Type-level natural for session types)
--------------------------------------------------------------------------------

-- | Priority level for session operations
-- Higher priority sessions get precedence; used for deadlock prevention
data Priority
    = PriorityZ                    -- ^ Base priority (zero)
    | PriorityS Priority           -- ^ Successor (one higher)
    deriving (Show, Eq, Ord, Generic)

instance ToJSON Priority where
    toJSON = toJSON . priorityToInt

instance FromJSON Priority where
    parseJSON v = priorityFromInt <$> JSON.parseJSON v

-- | Increment priority by one
succPriority :: Priority -> Priority
succPriority = PriorityS

-- | Convert priority to integer representation
priorityToInt :: Priority -> Int
priorityToInt PriorityZ = 0
priorityToInt (PriorityS p) = 1 + priorityToInt p

-- | Create priority from integer
priorityFromInt :: Int -> Priority
priorityFromInt n
    | n <= 0    = PriorityZ
    | otherwise = PriorityS (priorityFromInt (n - 1))

--------------------------------------------------------------------------------
-- Session Operations (for the REPL protocol)
--------------------------------------------------------------------------------

-- | Source document operations (read-only)
data SourceOp
    = PeekSection SectionId              -- ^ Get content of a section
    | GrepSource Pattern                 -- ^ Search for pattern in source
    | GetTheorem TheoremId               -- ^ Get theorem content
    | GetDependencies TheoremId          -- ^ Get theorem dependencies
    deriving (Show, Eq, Generic)

instance ToJSON SourceOp where
    toJSON (PeekSection sid) =
        JSON.object ["op" .= ("peek_section" :: Text), "sectionId" .= sid]
    toJSON (GrepSource pat) =
        JSON.object ["op" .= ("grep_source" :: Text), "pattern" .= pat]
    toJSON (GetTheorem tid) =
        JSON.object ["op" .= ("get_theorem" :: Text), "theoremId" .= tid]
    toJSON (GetDependencies tid) =
        JSON.object ["op" .= ("get_dependencies" :: Text), "theoremId" .= tid]

instance FromJSON SourceOp where
    parseJSON = JSON.withObject "SourceOp" $ \v -> do
        op <- v .: "op" :: Parser Text
        case op of
            "peek_section" -> PeekSection <$> v .: "sectionId"
            "grep_source" -> GrepSource <$> v .: "pattern"
            "get_theorem" -> GetTheorem <$> v .: "theoremId"
            "get_dependencies" -> GetDependencies <$> v .: "theoremId"
            _ -> fail $ "Unknown SourceOp: " <> T.unpack op

-- | Target (Agda) operations - delegates to agda-mcp
data TargetOp
    = AgdaLoadOp ModulePath              -- ^ Load module
    | AgdaGetGoalsOp                     -- ^ Get all goals
    | AgdaGetGoalContextOp HoleId        -- ^ Get context for goal
    | AgdaGiveOp HoleId Term             -- ^ Fill hole with term
    | AgdaRefineOp HoleId Term           -- ^ Refine hole with term
    | AgdaCaseSplitOp HoleId Var         -- ^ Case split on variable
    | AgdaAutoOp HoleId                  -- ^ Automatic proof search
    | AgdaSearchAboutOp Text             -- ^ Search definitions
    deriving (Show, Eq, Generic)

instance ToJSON TargetOp where
    toJSON (AgdaLoadOp modPath) =
        JSON.object ["op" .= ("agda_load" :: Text), "module" .= modPath]
    toJSON AgdaGetGoalsOp =
        JSON.object ["op" .= ("agda_get_goals" :: Text)]
    toJSON (AgdaGetGoalContextOp hid) =
        JSON.object ["op" .= ("agda_get_goal_context" :: Text), "holeId" .= hid]
    toJSON (AgdaGiveOp hid term) =
        JSON.object ["op" .= ("agda_give" :: Text), "holeId" .= hid, "term" .= term]
    toJSON (AgdaRefineOp hid term) =
        JSON.object ["op" .= ("agda_refine" :: Text), "holeId" .= hid, "term" .= term]
    toJSON (AgdaCaseSplitOp hid var) =
        JSON.object ["op" .= ("agda_case_split" :: Text), "holeId" .= hid, "variable" .= var]
    toJSON (AgdaAutoOp hid) =
        JSON.object ["op" .= ("agda_auto" :: Text), "holeId" .= hid]
    toJSON (AgdaSearchAboutOp query) =
        JSON.object ["op" .= ("agda_search_about" :: Text), "query" .= query]

instance FromJSON TargetOp where
    parseJSON = JSON.withObject "TargetOp" $ \v -> do
        op <- v .: "op" :: Parser Text
        case op of
            "agda_load" -> AgdaLoadOp <$> v .: "module"
            "agda_get_goals" -> pure AgdaGetGoalsOp
            "agda_get_goal_context" -> AgdaGetGoalContextOp <$> v .: "holeId"
            "agda_give" -> AgdaGiveOp <$> v .: "holeId" <*> v .: "term"
            "agda_refine" -> AgdaRefineOp <$> v .: "holeId" <*> v .: "term"
            "agda_case_split" -> AgdaCaseSplitOp <$> v .: "holeId" <*> v .: "variable"
            "agda_auto" -> AgdaAutoOp <$> v .: "holeId"
            "agda_search_about" -> AgdaSearchAboutOp <$> v .: "query"
            _ -> fail $ "Unknown TargetOp: " <> T.unpack op

-- | Bijection operations
data BijectionOp
    = SourceForHole HoleId               -- ^ Get source reference for hole
    | FormalForSection SectionId         -- ^ Get formal reference for section
    | GetCoverage                        -- ^ Get coverage statistics
    | UpdateBijection SourceRef FormalRef -- ^ Add correspondence
    deriving (Show, Eq, Generic)

instance ToJSON BijectionOp where
    toJSON (SourceForHole hid) =
        JSON.object ["op" .= ("source_for_hole" :: Text), "holeId" .= hid]
    toJSON (FormalForSection sid) =
        JSON.object ["op" .= ("formal_for_section" :: Text), "sectionId" .= sid]
    toJSON GetCoverage =
        JSON.object ["op" .= ("get_coverage" :: Text)]
    toJSON (UpdateBijection src formal) =
        JSON.object ["op" .= ("update_bijection" :: Text), "source" .= src, "formal" .= formal]

instance FromJSON BijectionOp where
    parseJSON = JSON.withObject "BijectionOp" $ \v -> do
        op <- v .: "op" :: Parser Text
        case op of
            "source_for_hole" -> SourceForHole <$> v .: "holeId"
            "formal_for_section" -> FormalForSection <$> v .: "sectionId"
            "get_coverage" -> pure GetCoverage
            "update_bijection" -> UpdateBijection <$> v .: "source" <*> v .: "formal"
            _ -> fail $ "Unknown BijectionOp: " <> T.unpack op

-- | All session operations
data SessionOp
    = OpSource SourceOp
    | OpTarget TargetOp
    | OpBijection BijectionOp
    | OpRecurse Task                     -- ^ Recursive call with sub-task
    | OpFinal Output                     -- ^ Terminate session
    deriving (Show, Eq, Generic)

instance ToJSON SessionOp where
    toJSON (OpSource op) =
        JSON.object ["category" .= ("source" :: Text), "operation" .= op]
    toJSON (OpTarget op) =
        JSON.object ["category" .= ("target" :: Text), "operation" .= op]
    toJSON (OpBijection op) =
        JSON.object ["category" .= ("bijection" :: Text), "operation" .= op]
    toJSON (OpRecurse task) =
        JSON.object ["category" .= ("recurse" :: Text), "task" .= task]
    toJSON (OpFinal output) =
        JSON.object ["category" .= ("final" :: Text), "output" .= output]

instance FromJSON SessionOp where
    parseJSON = JSON.withObject "SessionOp" $ \v -> do
        cat <- v .: "category" :: Parser Text
        case cat of
            "source" -> OpSource <$> v .: "operation"
            "target" -> OpTarget <$> v .: "operation"
            "bijection" -> OpBijection <$> v .: "operation"
            "recurse" -> OpRecurse <$> v .: "task"
            "final" -> OpFinal <$> v .: "output"
            _ -> fail $ "Unknown category: " <> T.unpack cat
