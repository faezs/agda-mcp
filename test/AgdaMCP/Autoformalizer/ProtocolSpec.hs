{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the Autoformalizer REPL Protocol
--
-- This module tests the protocol implementation against the specification:
--   * Type serialization/deserialization
--   * Source document operations
--   * Bijection state management
--   * Session type invariants
--   * Priority management

module AgdaMCP.Autoformalizer.ProtocolSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Aeson (encode, decode, ToJSON, FromJSON)
import qualified Data.Aeson as JSON
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import AgdaMCP.Autoformalizer.Types
import AgdaMCP.Autoformalizer.Source
import AgdaMCP.Autoformalizer.Bijection
import AgdaMCP.Autoformalizer.Session
import AgdaMCP.Autoformalizer.Handler
import AgdaMCP.Autoformalizer.Protocol

-- | All autoformalizer protocol tests
tests :: TestTree
tests = testGroup "Autoformalizer Protocol"
    [ typeSerializationTests
    , sourceOperationTests
    , bijectionTests
    , sessionTests
    , priorityTests
    , handlerTests
    ]

--------------------------------------------------------------------------------
-- Type Serialization Tests
--------------------------------------------------------------------------------

typeSerializationTests :: TestTree
typeSerializationTests = testGroup "Type Serialization"
    [ testCase "SectionId roundtrip" $ do
        let sid = SectionId "section-1.2.3"
        roundtripJSON sid

    , testCase "TheoremId roundtrip" $ do
        let tid = TheoremId "thm-main"
        roundtripJSON tid

    , testCase "HoleId roundtrip" $ do
        let hid = HoleId 42
        roundtripJSON hid

    , testCase "SourceContent roundtrip" $ do
        let content = SourceContent
                { sourceText = "Test content with math $x^2$"
                , sourceMath = [MathExpr "x^2" (23, 28)]
                }
        roundtripJSON content

    , testCase "Match roundtrip" $ do
        let match = Match (SectionId "sec1") (10, 20) "sample snippet"
        roundtripJSON match

    , testCase "LoadResult success roundtrip" $ do
        let result = LoadSuccess [Goal (HoleId 0) "Nat" []]
        roundtripJSON result

    , testCase "LoadResult error roundtrip" $ do
        let result = LoadError "Type error"
        roundtripJSON result

    , testCase "GiveResult variants roundtrip" $ do
        roundtripJSON GiveSuccess
        roundtripJSON (GiveRefined [HoleId 1, HoleId 2])
        roundtripJSON (GiveError "Cannot unify")

    , testCase "Focus variants roundtrip" $ do
        roundtripJSON (FillHole (HoleId 0))
        roundtripJSON (FormalizeSection (SectionId "intro"))
        roundtripJSON (ProvePostulate (Name "axiom-K"))

    , testCase "Task roundtrip" $ do
        let task = Task
                { taskFocus = FillHole (HoleId 0)
                , taskSourceSlice = Just (SectionId "2.1", SourceContent "text" [])
                , taskTargetModule = Just (ModulePath "Data.Nat")
                , taskRelevantBijection =
                    [ ( SourceRef (SectionId "1.1") (0, 100)
                      , FormalRef (ModulePath "Data.Nat") (Name "Nat")
                      )
                    ]
                }
        roundtripJSON task

    , testCase "Status variants roundtrip" $ do
        roundtripJSON Success
        roundtripJSON (Partial "incomplete")
        roundtripJSON (Failed "error message")

    , testCase "Effect variants roundtrip" $ do
        roundtripJSON (HoleFilled (HoleId 0) (Term "zero"))
        roundtripJSON (BijectionUpdated
            (SourceRef (SectionId "s1") (0, 10))
            (FormalRef (ModulePath "M") (Name "n")))
        roundtripJSON (ModuleModified (ModulePath "M"))

    , testCase "SessionOp roundtrip" $ do
        roundtripJSON (OpSource (PeekSection (SectionId "intro")))
        roundtripJSON (OpTarget AgdaGetGoalsOp)
        roundtripJSON (OpBijection GetCoverage)
        roundtripJSON (OpFinal (Output Nothing [] Success))

    , testCase "Priority roundtrip" $ do
        roundtripJSON PriorityZ
        roundtripJSON (PriorityS (PriorityS PriorityZ))
        assertEqual "Priority 5" 5 (priorityToInt (priorityFromInt 5))
    ]

-- | Helper to test JSON roundtrip
roundtripJSON :: (Eq a, Show a, ToJSON a, FromJSON a) => a -> Assertion
roundtripJSON x = do
    let encoded = encode x
        decoded = decode encoded
    assertEqual ("Roundtrip for " <> show x) (Just x) decoded

--------------------------------------------------------------------------------
-- Source Operation Tests
--------------------------------------------------------------------------------

sourceOperationTests :: TestTree
sourceOperationTests = testGroup "Source Operations"
    [ testCase "Load empty document" $ do
        let doc = loadSourceDocument ""
        assertEqual "Empty sections" [] (sourceSectionOrder doc)

    , testCase "Load plain text document" $ do
        let doc = loadSourceDocument "Just plain text"
            sections = sourceSectionOrder doc
        assertEqual "One section" 1 (length sections)

    , testCase "Load markdown document" $ do
        let content = T.unlines
                [ "# Introduction"
                , "This is the intro."
                , ""
                , "## Background"
                , "Some background."
                , ""
                , "# Methods"
                , "The methods section."
                ]
            doc = loadSourceDocument content
        assertEqual "Has sections" True (not $ null $ sourceSectionOrder doc)

    , testCase "Load LaTeX document" $ do
        let content = T.unlines
                [ "\\section{Introduction}"
                , "This is the intro."
                , ""
                , "\\subsection{Background}"
                , "Some background."
                ]
            doc = loadSourceDocument content
        assertEqual "Has sections" True (not $ null $ sourceSectionOrder doc)

    , testCase "Peek section returns content" $ do
        let doc = loadSourceDocument "# Test\nContent here"
            sections = sourceSectionOrder doc
        case sections of
            [] -> assertFailure "No sections found"
            (sid:_) -> do
                case peekSection doc sid of
                    Left err -> assertFailure $ T.unpack err
                    Right content -> do
                        assertEqual "Content not empty" True (not $ T.null $ sourceText content)

    , testCase "Grep source finds matches" $ do
        let doc = loadSourceDocument "Hello world. Hello again."
            matches = grepSource doc (Pattern "Hello")
        assertEqual "Found 2 matches" 2 (length matches)

    , testCase "Extract inline math" $ do
        let exprs = extractMathExpressions "Some text $x^2 + y^2$ and $z$"
        assertEqual "Found 2 math expressions" 2 (length exprs)

    , testCase "Extract display math" $ do
        let exprs = extractMathExpressions "Before $$E = mc^2$$ after"
        assertEqual "Found 1 display math" 1 (length exprs)
    ]

--------------------------------------------------------------------------------
-- Bijection Tests
--------------------------------------------------------------------------------

bijectionTests :: TestTree
bijectionTests = testGroup "Bijection Operations"
    [ testCase "Empty bijection" $ do
        let bij = emptyBijection
        assertEqual "No entries" 0 (length $ allEntries bij)
        assertEqual "No source for hole" Nothing (sourceForHole bij (HoleId 0))

    , testCase "Update bijection" $ do
        let srcRef = SourceRef (SectionId "s1") (0, 100)
            formalRef = FormalRef (ModulePath "M") (Name "f")
            bij = updateBijection emptyBijection srcRef formalRef
        assertEqual "One entry" 1 (length $ allEntries bij)

    , testCase "Source for hole lookup" $ do
        let srcRef = SourceRef (SectionId "s1") (0, 100)
            formalRef = FormalRef (ModulePath "M") (Name "f")
            entry = BijectionEntry
                { entrySource = srcRef
                , entryFormal = formalRef
                , entryConfidence = 1.0
                , entryNotes = Nothing
                , entryHoles = [HoleId 0, HoleId 1]
                }
            bij0 = emptyBijection
            -- Manually add entry with holes
            bij1 = bij0
                { bijBySource = Map.singleton srcRef entry
                , bijByFormal = Map.singleton formalRef entry
                , bijHoleToSource = Map.fromList [(HoleId 0, srcRef), (HoleId 1, srcRef)]
                }
        assertEqual "Found source for hole 0" (Just srcRef) (sourceForHole bij1 (HoleId 0))
        assertEqual "Found source for hole 1" (Just srcRef) (sourceForHole bij1 (HoleId 1))
        assertEqual "No source for hole 2" Nothing (sourceForHole bij1 (HoleId 2))

    , testCase "Formal for section lookup" $ do
        let srcRef = SourceRef (SectionId "s1") (0, 100)
            formalRef = FormalRef (ModulePath "M") (Name "f")
            bij = updateBijection emptyBijection srcRef formalRef
        assertEqual "Found formal" (Just formalRef) (formalForSection bij (SectionId "s1"))
        assertEqual "No formal for s2" Nothing (formalForSection bij (SectionId "s2"))

    , testCase "Coverage calculation" $ do
        let bij0 = emptyBijection
                { bijAllSections = Set.fromList [SectionId "s1", SectionId "s2", SectionId "s3"]
                , bijAllHoles = Set.fromList [HoleId 0, HoleId 1]
                , bijAllPostulates = Set.fromList [Name "p1"]
                }
            srcRef = SourceRef (SectionId "s1") (0, 100)
            formalRef = FormalRef (ModulePath "M") (Name "f")
            bij1 = updateBijection bij0 srcRef formalRef
            coverage = getCoverage bij1
        assertEqual "33% coverage" 33 (coveragePercent coverage)
        assertEqual "2 holes" 2 (coverageHolesRemaining coverage)
        assertEqual "1 postulate" 1 (coveragePostulatesRemaining coverage)
        assertEqual "2 uncovered" 2 (length $ coverageUncoveredSections coverage)

    , testCase "Bijection persistence roundtrip" $ do
        let srcRef = SourceRef (SectionId "s1") (0, 100)
            formalRef = FormalRef (ModulePath "M") (Name "f")
            bij = updateBijection emptyBijection srcRef formalRef
        -- Test by checking JSON roundtrip of the entries
        let entries = allEntries bij
        mapM_ roundtripJSON entries
    ]

--------------------------------------------------------------------------------
-- Session Tests
--------------------------------------------------------------------------------

sessionTests :: TestTree
sessionTests = testGroup "Session Management"
    [ testCase "Create new session" $ do
        srcRef <- newIORef Nothing
        bijRef <- newIORef emptyBijection
        session <- newSessionState defaultSessionConfig PriorityZ Nothing srcRef bijRef
        priority <- atomically $ readTVar (sessionPriority session)
        assertEqual "Initial priority is Z" PriorityZ priority

    , testCase "Session token has unique ID" $ do
        srcRef <- newIORef Nothing
        bijRef <- newIORef emptyBijection
        s1 <- newSessionState defaultSessionConfig PriorityZ Nothing srcRef bijRef
        s2 <- newSessionState defaultSessionConfig PriorityZ Nothing srcRef bijRef
        assertBool "Different IDs" (tokenId (sessionToken s1) /= tokenId (sessionToken s2))

    , testCase "Session can continue check" $ do
        srcRef <- newIORef Nothing
        bijRef <- newIORef emptyBijection
        session <- newSessionState defaultSessionConfig PriorityZ Nothing srcRef bijRef
        canCont <- canContinue session
        assertEqual "Can continue initially" True canCont

    , testCase "Session finalization prevents continuation" $ do
        srcRef <- newIORef Nothing
        bijRef <- newIORef emptyBijection
        session <- newSessionState defaultSessionConfig PriorityZ Nothing srcRef bijRef
        atomically $ writeTVar (sessionFinalized session) True
        canCont <- canContinue session
        assertEqual "Cannot continue after finalization" False canCont

    , testCase "Priority increments by 2" $ do
        srcRef <- newIORef Nothing
        bijRef <- newIORef emptyBijection
        session <- newSessionState defaultSessionConfig PriorityZ Nothing srcRef bijRef
        _ <- incrementPriority session
        newPriority <- atomically $ readTVar (sessionPriority session)
        assertEqual "Priority is 2" 2 (priorityToInt newPriority)
    ]

--------------------------------------------------------------------------------
-- Priority Tests
--------------------------------------------------------------------------------

priorityTests :: TestTree
priorityTests = testGroup "Priority Management"
    [ testCase "Priority Z is 0" $ do
        assertEqual "Z = 0" 0 (priorityToInt PriorityZ)

    , testCase "Priority S increments" $ do
        assertEqual "S Z = 1" 1 (priorityToInt (PriorityS PriorityZ))
        assertEqual "S S Z = 2" 2 (priorityToInt (PriorityS (PriorityS PriorityZ)))

    , testCase "Priority from int" $ do
        assertEqual "fromInt 0" PriorityZ (priorityFromInt 0)
        assertEqual "fromInt 3" (PriorityS (PriorityS (PriorityS PriorityZ))) (priorityFromInt 3)

    , testCase "Priority roundtrip" $ do
        let testPriority n = assertEqual ("Priority " <> show n) n (priorityToInt (priorityFromInt n))
        mapM_ testPriority [0, 1, 5, 10, 50, 100, 255]

    , testCase "Succ priority increments by 1" $ do
        let p0 = PriorityZ
            p1 = succPriority p0
            p2 = succPriority p1
        assertEqual "succ Z = 1" 1 (priorityToInt p1)
        assertEqual "succ (succ Z) = 2" 2 (priorityToInt p2)
    ]

--------------------------------------------------------------------------------
-- Handler Tests
--------------------------------------------------------------------------------

handlerTests :: TestTree
handlerTests = testGroup "Handler Operations"
    [ testCase "Create handler with defaults" $ do
        handler <- newHandler defaultHandlerConfig
        -- Just verify it doesn't crash
        return ()

    , testCase "Source operations on empty document" $ do
        handler <- newHandler defaultHandlerConfig
        result <- runSourceOp handler (PeekSection (SectionId "nonexistent"))
        case result of
            PeekSectionResult content ->
                assertEqual "Empty content" "" (sourceText content)
            _ -> assertFailure "Expected PeekSectionResult"

    , testCase "Bijection operations" $ do
        handler <- newHandler defaultHandlerConfig

        -- Get coverage (should be 100% with no sections)
        result <- runBijectionOp handler GetCoverage
        case result of
            GetCoverageResult coverage ->
                assertEqual "100% coverage (no sections)" 100 (coveragePercent coverage)
            _ -> assertFailure "Expected GetCoverageResult"

    , testCase "Simple policy terminates" $ do
        handler <- newHandler defaultHandlerConfig
        let task = Task
                { taskFocus = FillHole (HoleId 0)
                , taskSourceSlice = Nothing
                , taskTargetModule = Nothing
                , taskRelevantBijection = []
                }
        result <- runProtocol handler task simplePolicy
        -- Should terminate (may fail since not connected to agda-mcp)
        return ()

    , testCase "Auto fill policy terminates" $ do
        handler <- newHandler defaultHandlerConfig
        let task = Task
                { taskFocus = FillHole (HoleId 0)
                , taskSourceSlice = Nothing
                , taskTargetModule = Nothing
                , taskRelevantBijection = []
                }
        result <- runProtocol handler task autoFillPolicy
        -- Should terminate with partial result
        case resultStatus result of
            Success -> return ()
            Partial _ -> return ()
            Failed _ -> return ()  -- Expected since not connected to agda-mcp
    ]
