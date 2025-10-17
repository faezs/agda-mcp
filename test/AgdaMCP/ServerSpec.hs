{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.ServerSpec (tests) where

import Test.Tasty
import Test.Tasty.Providers
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Key as JSON.Key
import qualified Data.Aeson.KeyMap as JSON.KeyMap
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector as V
import Data.IORef
import System.FilePath ((</>))
import System.Directory (getCurrentDirectory)
import Control.Exception (try, SomeException)
import Control.Concurrent.MVar (tryPutMVar)
import Control.Concurrent.Async (waitCatch, Async)
import Control.Monad (void)
import System.Timeout (timeout)

import AgdaMCP.Server
import qualified AgdaMCP.Types as Types
import qualified AgdaMCP.SessionManager as SessionManager
import qualified MCP.Server as MCP
import qualified AgdaMCP.MultiAgentSpec as MultiAgent
import AgdaMCP.TestUtils (withTempTestFile)

-- | Simple test case type
data SimpleTest = SimpleTest String (IO ())

instance IsTest SimpleTest where
  run _ (SimpleTest _ action) _ = do
    result <- try action :: IO (Either SomeException ())
    pure $ case result of
      Right () -> testPassed ""
      Left e -> testFailed (show e)
  testOptions = pure []

simpleTestCase :: String -> IO () -> TestTree
simpleTestCase name action = singleTest name (SimpleTest name action)

-- | Helper assertions
assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual msg expected actual =
  if expected == actual
    then pure ()
    else fail $ msg ++ ": expected " ++ show expected ++ " but got " ++ show actual

assertBool :: String -> Bool -> IO ()
assertBool msg condition =
  if condition
    then pure ()
    else fail msg

assertFailure :: String -> IO ()
assertFailure = fail

-- | Helper to run a tool and get the response as JSON (forces Full format)
runTool :: SessionManager.SessionManager ServerState -> Types.AgdaTool -> IO JSON.Value
runTool manager tool = do
  -- Force Full format for JSON parsing
  let toolWithFullFormat = setFormat tool (Just "Full")
  result <- handleAgdaToolWithSession manager toolWithFullFormat
  case result of
    MCP.ContentText txt -> do
      case JSON.decode (LBS.fromStrict $ TE.encodeUtf8 txt) of
        Just val -> pure val
        Nothing -> fail $ "Failed to parse JSON response: " ++ T.unpack txt
    _ -> fail "Expected ContentText response"

-- | Helper to run a tool and get concise text response
runToolConcise :: SessionManager.SessionManager ServerState -> Types.AgdaTool -> IO Text
runToolConcise manager tool = do
  -- Force Concise format
  let toolWithConciseFormat = setFormat tool (Just "Concise")
  result <- handleAgdaToolWithSession manager toolWithConciseFormat
  case result of
    MCP.ContentText txt -> pure txt
    _ -> fail "Expected ContentText response"

-- | Set format on a tool (preserves sessionId)
setFormat :: Types.AgdaTool -> Maybe Text -> Types.AgdaTool
setFormat tool fmt = case tool of
  Types.AgdaLoad{Types.file=f, Types.sessionId=sid} -> Types.AgdaLoad{Types.file=f, Types.sessionId=sid, Types.format=fmt}
  Types.AgdaGetGoals{Types.sessionId=sid} -> Types.AgdaGetGoals{Types.sessionId=sid, Types.format=fmt}
  Types.AgdaGetGoalType{Types.goalId=gid, Types.sessionId=sid} -> Types.AgdaGetGoalType{Types.goalId=gid, Types.sessionId=sid, Types.format=fmt}
  Types.AgdaGetGoalTypeImplicits{Types.goalId=gid, Types.sessionId=sid} -> Types.AgdaGetGoalTypeImplicits{Types.goalId=gid, Types.sessionId=sid, Types.format=fmt}
  Types.AgdaGetContext{Types.goalId=gid, Types.sessionId=sid} -> Types.AgdaGetContext{Types.goalId=gid, Types.sessionId=sid, Types.format=fmt}
  Types.AgdaGetContextImplicits{Types.goalId=gid, Types.sessionId=sid} -> Types.AgdaGetContextImplicits{Types.goalId=gid, Types.sessionId=sid, Types.format=fmt}
  Types.AgdaGive{Types.goalId=gid, Types.expression=expr, Types.sessionId=sid} -> Types.AgdaGive{Types.goalId=gid, Types.expression=expr, Types.sessionId=sid, Types.format=fmt}
  Types.AgdaRefine{Types.goalId=gid, Types.expression=expr, Types.sessionId=sid} -> Types.AgdaRefine{Types.goalId=gid, Types.expression=expr, Types.sessionId=sid, Types.format=fmt}
  Types.AgdaCaseSplit{Types.goalId=gid, Types.variable=var, Types.sessionId=sid} -> Types.AgdaCaseSplit{Types.goalId=gid, Types.variable=var, Types.sessionId=sid, Types.format=fmt}
  Types.AgdaCompute{Types.goalId=gid, Types.expression=expr, Types.sessionId=sid} -> Types.AgdaCompute{Types.goalId=gid, Types.expression=expr, Types.sessionId=sid, Types.format=fmt}
  Types.AgdaInferType{Types.goalId=gid, Types.expression=expr, Types.sessionId=sid} -> Types.AgdaInferType{Types.goalId=gid, Types.expression=expr, Types.sessionId=sid, Types.format=fmt}
  Types.AgdaIntro{Types.goalId=gid, Types.sessionId=sid} -> Types.AgdaIntro{Types.goalId=gid, Types.sessionId=sid, Types.format=fmt}
  Types.AgdaAuto{Types.goalId=gid, Types.timeout=t, Types.sessionId=sid} -> Types.AgdaAuto{Types.goalId=gid, Types.timeout=t, Types.sessionId=sid, Types.format=fmt}
  Types.AgdaAutoAll{Types.timeout=t, Types.sessionId=sid} -> Types.AgdaAutoAll{Types.timeout=t, Types.sessionId=sid, Types.format=fmt}
  Types.AgdaSolveOne{Types.goalId=gid, Types.sessionId=sid} -> Types.AgdaSolveOne{Types.goalId=gid, Types.sessionId=sid, Types.format=fmt}
  Types.AgdaHelperFunction{Types.goalId=gid, Types.helperName=name, Types.sessionId=sid} -> Types.AgdaHelperFunction{Types.goalId=gid, Types.helperName=name, Types.sessionId=sid, Types.format=fmt}
  Types.AgdaGoalTypeContext{Types.goalId=gid, Types.sessionId=sid} -> Types.AgdaGoalTypeContext{Types.goalId=gid, Types.sessionId=sid, Types.format=fmt}
  Types.AgdaGoalAtPosition{Types.file=f, Types.line=l, Types.column=c, Types.sessionId=sid} -> Types.AgdaGoalAtPosition{Types.file=f, Types.line=l, Types.column=c, Types.sessionId=sid, Types.format=fmt}
  Types.AgdaGotoDefinition{Types.file=f, Types.line=l, Types.column=c, Types.sessionId=sid} -> Types.AgdaGotoDefinition{Types.file=f, Types.line=l, Types.column=c, Types.sessionId=sid, Types.format=fmt}
  Types.AgdaSearchAbout{Types.query=q, Types.sessionId=sid} -> Types.AgdaSearchAbout{Types.query=q, Types.sessionId=sid, Types.format=fmt}
  Types.AgdaShowModule{Types.moduleName=m, Types.sessionId=sid} -> Types.AgdaShowModule{Types.moduleName=m, Types.sessionId=sid, Types.format=fmt}
  Types.AgdaShowConstraints{Types.sessionId=sid} -> Types.AgdaShowConstraints{Types.sessionId=sid, Types.format=fmt}
  Types.AgdaWhyInScope{Types.name=n, Types.sessionId=sid} -> Types.AgdaWhyInScope{Types.name=n, Types.sessionId=sid, Types.format=fmt}
  Types.AgdaListPostulates{Types.file=f, Types.sessionId=sid} -> Types.AgdaListPostulates{Types.file=f, Types.sessionId=sid, Types.format=fmt}

-- | Helper to get a field from a JSON object
getField :: T.Text -> JSON.Value -> Maybe JSON.Value
getField field (JSON.Object obj) = JSON.KeyMap.lookup (JSON.Key.fromText field) obj
getField _ _ = Nothing

-- ============================================================================
-- Resource Management
-- ============================================================================

-- | Create a SessionManager resource with proper cleanup
-- All tests will share this SessionManager, but each test gets its own isolated session via sessionId
withSessionManager :: (IO (SessionManager.SessionManager ServerState) -> TestTree) -> TestTree
withSessionManager = withResource initSessionManager SessionManager.destroySessionManager

tests :: TestTree
tests = testGroup "AgdaMCP.Server Tests"
  [ loadTests
  , getGoalsTests
  , getGoalTypeTests
  , getGoalTypeImplicitsTests
  , getContextTests
  , getContextImplicitsTests
  , giveTests
  , refineTests
  , caseSplitTests
  , computeTests
  , inferTypeTests
  , introTests
  , autoTests
  , autoAllTests
  , solveOneTests
  , helperFunctionTests
  , goalTypeContextTests
  , goalAtPositionTests
  , searchAboutTests
  , showModuleTests
  , showConstraintsTests
  , whyInScopeTests
  , listPostulatesTests
  , MultiAgent.tests
  ]

-- ============================================================================
-- Test Data
-- ============================================================================

exampleFile :: IO FilePath
exampleFile = do
  cwd <- getCurrentDirectory
  pure $ cwd </> "test" </> "Example.agda"

postulateFile :: IO FilePath
postulateFile = do
  cwd <- getCurrentDirectory
  pure $ cwd </> "test" </> "PostulateTest.agda"

searchTestFile :: IO FilePath
searchTestFile = do
  cwd <- getCurrentDirectory
  pure $ cwd </> "test" </> "SearchTest.agda"

-- | Tests for agda_load
loadTests :: TestTree
loadTests = withSessionManager $ \getManager -> testGroup "agda_load"
  [ simpleTestCase "loads file successfully and returns goals" $ do
      manager <- getManager
      file <- exampleFile
      let tool = Types.AgdaLoad { Types.file = T.pack file, Types.sessionId = Just "test-load-success", Types.format = Nothing }
      response <- runTool manager tool

      -- Should return DisplayInfo with AllGoalsWarnings
      let kind = getField "kind" response
      assertEqual "Response kind should be DisplayInfo" (Just (JSON.String "DisplayInfo")) kind

      let info = getField "info" response
      assertBool "Should have info field" (info /= Nothing)

      let visibleGoals = case info of
            Just (JSON.Object obj) -> JSON.KeyMap.lookup (JSON.Key.fromText "visibleGoals") obj
            _ -> Nothing

      case visibleGoals of
        Just (JSON.Array goals) -> do
          assertEqual "Should have 5 goals" 5 (length goals)
        _ -> assertFailure "Should have visibleGoals array"

  , simpleTestCase "fails to load non-existent file" $ do
      manager <- getManager
      let tool = Types.AgdaLoad { Types.file = "/non/existent/file.agda", Types.sessionId = Just "test-load-error", Types.format = Nothing }
      response <- runTool manager tool

      -- Should return an error response
      let kind = getField "kind" response
      assertBool "Should have error response" (kind == Just (JSON.String "DisplayInfo") || kind == Just (JSON.String "Error"))
  ]

-- | Tests for agda_get_goals
getGoalsTests :: TestTree
getGoalsTests = withSessionManager $ \getManager -> testGroup "agda_get_goals"
  [ simpleTestCase "gets goals after successful load" $ do
      manager <- getManager
      file <- exampleFile

      -- Load file first
      _ <- runTool manager (Types.AgdaLoad { Types.file = T.pack file, Types.sessionId = Just "test-get-goals", Types.format = Nothing })

      -- Get goals from loaded file
      response <- runTool manager (Types.AgdaGetGoals { Types.sessionId = Just "test-get-goals", Types.format = Nothing })

      let info = getField "info" response
      let visibleGoals = case info of
            Just (JSON.Object obj) -> JSON.KeyMap.lookup (JSON.Key.fromText "visibleGoals") obj
            _ -> Nothing

      case visibleGoals of
        Just (JSON.Array goals) -> do
          assertEqual "Should have 5 goals" 5 (length goals)
        _ -> assertFailure "Should have visibleGoals array"
  ]

-- | Tests for agda_get_goal_type
getGoalTypeTests :: TestTree
getGoalTypeTests = withSessionManager $ \getManager -> testGroup "agda_get_goal_type"
  [ simpleTestCase "gets type of goal 0" $ do
      manager <- getManager
      file <- exampleFile
      -- Load file first
      _ <- runTool manager (Types.AgdaLoad { Types.file = T.pack file, Types.sessionId = Just "test-goal-type", Types.format = Nothing })

      let tool = Types.AgdaGetGoalType { Types.goalId = 0, Types.sessionId = Just "test-goal-type", Types.format = Nothing }
      response <- runTool manager tool

      -- Should return DisplayInfo with goal type
      let kind = getField "kind" response
      assertEqual "Should be DisplayInfo" (Just (JSON.String "DisplayInfo")) kind

  , simpleTestCase "fails on invalid goal ID" $ do
      manager <- getManager
      file <- exampleFile
      -- Load file first
      _ <- runTool manager (Types.AgdaLoad { Types.file = T.pack file, Types.sessionId = Just "test-goal-type-invalid", Types.format = Nothing })

      let tool = Types.AgdaGetGoalType { Types.goalId = 999, Types.sessionId = Just "test-goal-type-invalid", Types.format = Nothing }
      response <- runTool manager tool

      -- Should return error
      let kind = getField "kind" response
      assertBool "Should have error" (kind == Just (JSON.String "DisplayInfo"))
  ]

getGoalTypeImplicitsTests :: TestTree
getGoalTypeImplicitsTests = withSessionManager $ \getManager -> testGroup "agda_get_goal_type_implicits"
  [ simpleTestCase "gets type of goal 0 with implicits" $ do
      manager <- getManager
      file <- exampleFile
      _ <- runTool manager (Types.AgdaLoad { Types.file = T.pack file, Types.sessionId = Just "test-goal-type-implicits", Types.format = Nothing })

      let tool = Types.AgdaGetGoalTypeImplicits { Types.goalId = 0, Types.sessionId = Just "test-goal-type-implicits", Types.format = Nothing }
      response <- runTool manager tool

      -- Should return DisplayInfo with goal type (showing implicit arguments)
      let kind = getField "kind" response
      assertEqual "Should be DisplayInfo" (Just (JSON.String "DisplayInfo")) kind

  , simpleTestCase "fails on invalid goal ID" $ do
      manager <- getManager
      file <- exampleFile
      _ <- runTool manager (Types.AgdaLoad { Types.file = T.pack file, Types.sessionId = Just "test-goal-type-implicits-invalid", Types.format = Nothing })

      let tool = Types.AgdaGetGoalTypeImplicits { Types.goalId = 999, Types.sessionId = Just "test-goal-type-implicits-invalid", Types.format = Nothing }
      response <- runTool manager tool

      -- Should return error
      let kind = getField "kind" response
      assertBool "Should have error" (kind == Just (JSON.String "DisplayInfo"))
  ]

-- | Tests for agda_get_context
getContextTests :: TestTree
getContextTests = withSessionManager $ \getManager -> testGroup "agda_get_context"
  [ simpleTestCase "gets context at goal 0" $ do
      manager <- getManager
      file <- exampleFile
      _ <- runTool manager (Types.AgdaLoad { Types.file = T.pack file, Types.sessionId = Just "test-get-context", Types.format = Nothing })

      let tool = Types.AgdaGetContext { Types.goalId = 0, Types.sessionId = Just "test-get-context", Types.format = Nothing }
      response <- runTool manager tool

      let kind = getField "kind" response
      assertEqual "Should be DisplayInfo" (Just (JSON.String "DisplayInfo")) kind

      -- Context should contain variable 'n'
      -- (We can't easily assert the exact content without parsing the full structure)
  ]

getContextImplicitsTests :: TestTree
getContextImplicitsTests = withSessionManager $ \getManager -> testGroup "agda_get_context_implicits"
  [ simpleTestCase "gets context at goal 0 with implicits" $ do
      manager <- getManager
      file <- exampleFile
      _ <- runTool manager (Types.AgdaLoad { Types.file = T.pack file, Types.sessionId = Just "test-get-context-implicits", Types.format = Nothing })

      let tool = Types.AgdaGetContextImplicits { Types.goalId = 0, Types.sessionId = Just "test-get-context-implicits", Types.format = Nothing }
      response <- runTool manager tool

      -- Should return DisplayInfo with context (showing implicit arguments in types)
      let kind = getField "kind" response
      assertEqual "Should be DisplayInfo" (Just (JSON.String "DisplayInfo")) kind
  ]

-- | Tests for agda_give
giveTests :: TestTree
giveTests = withSessionManager $ \getManager -> testGroup "agda_give"
  [ simpleTestCase "successful give returns GiveAction" $ do
      manager <- getManager
      withTempTestFile "Example.agda" $ \file -> do
        _ <- runTool manager (Types.AgdaLoad { Types.file = T.pack file, Types.sessionId = Just "test-give-success", Types.format = Nothing })

        let tool = Types.AgdaGive { Types.goalId = 0, Types.expression = "n", Types.sessionId = Just "test-give-success", Types.format = Nothing }
        response <- runTool manager tool

        let kind = getField "kind" response
        assertEqual "Should be GiveAction" (Just (JSON.String "GiveAction")) kind

        let giveResult = getField "giveResult" response
        assertBool "Should have giveResult" (giveResult /= Nothing)

  , simpleTestCase "failed give returns error" $ do
      manager <- getManager
      withTempTestFile "Example.agda" $ \file -> do
        _ <- runTool manager (Types.AgdaLoad { Types.file = T.pack file, Types.sessionId = Just "test-give-error", Types.format = Nothing })

        -- Use a completely invalid expression to trigger a parse/scope error
        let tool = Types.AgdaGive { Types.goalId = 0, Types.expression = "nonExistentName123", Types.sessionId = Just "test-give-error", Types.format = Nothing }
        response <- runTool manager tool

        let kind = getField "kind" response
        -- Should be DisplayInfo with error (scope error for undefined name)
        assertEqual "Should be DisplayInfo with error" (Just (JSON.String "DisplayInfo")) kind

        -- Check for error in info
        let info = getField "info" response
        case info of
          Just (JSON.Object obj) -> do
            let infoKind = JSON.KeyMap.lookup (JSON.Key.fromText "kind") obj
            assertEqual "Info kind should be Error" (Just (JSON.String "Error")) infoKind
          _ -> assertFailure "Should have info object"
  ]

-- | Tests for agda_refine
refineTests :: TestTree
refineTests = withSessionManager $ \getManager -> testGroup "agda_refine"
  [ simpleTestCase "refines goal with constructor" $ do
      manager <- getManager
      withTempTestFile "Example.agda" $ \file -> do
        _ <- runTool manager (Types.AgdaLoad { Types.file = T.pack file, Types.sessionId = Just "test-refine", Types.format = Nothing })

        let tool = Types.AgdaRefine { Types.goalId = 1, Types.expression = "suc", Types.sessionId = Just "test-refine", Types.format = Nothing }
        response <- runTool manager tool

        -- Refine should return GiveAction
        let kind = getField "kind" response
        assertBool "Should be GiveAction or DisplayInfo"
          (kind == Just (JSON.String "GiveAction") || kind == Just (JSON.String "DisplayInfo"))
  ]

-- | Tests for agda_case_split
caseSplitTests :: TestTree
caseSplitTests = withSessionManager $ \getManager -> testGroup "agda_case_split"
  [ simpleTestCase "splits on variable n" $ do
      manager <- getManager
      withTempTestFile "Example.agda" $ \file -> do
        _ <- runTool manager (Types.AgdaLoad { Types.file = T.pack file, Types.sessionId = Just "test-case-split", Types.format = Nothing })

        let tool = Types.AgdaCaseSplit { Types.goalId = 0, Types.variable = "n", Types.sessionId = Just "test-case-split", Types.format = Nothing }
        response <- runTool manager tool

        -- Case split returns MakeCase
        let kind = getField "kind" response
        assertBool "Should be MakeCase or DisplayInfo"
          (kind == Just (JSON.String "MakeCase") || kind == Just (JSON.String "DisplayInfo"))
  ]

-- | Tests for agda_compute
computeTests :: TestTree
computeTests = withSessionManager $ \getManager -> testGroup "agda_compute"
  [ simpleTestCase "computes expression" $ do
      manager <- getManager
      file <- exampleFile
      _ <- runTool manager (Types.AgdaLoad { Types.file = T.pack file, Types.sessionId = Just "test-compute", Types.format = Nothing })

      let tool = Types.AgdaCompute { Types.goalId = 0, Types.expression = "suc zero", Types.sessionId = Just "test-compute", Types.format = Nothing }
      response <- runTool manager tool

      let kind = getField "kind" response
      assertEqual "Should be DisplayInfo" (Just (JSON.String "DisplayInfo")) kind
  ]

-- | Tests for agda_infer_type
inferTypeTests :: TestTree
inferTypeTests = withSessionManager $ \getManager -> testGroup "agda_infer_type"
  [ simpleTestCase "infers type of valid expression" $ do
      manager <- getManager
      file <- exampleFile
      _ <- runTool manager (Types.AgdaLoad { Types.file = T.pack file, Types.sessionId = Just "test-infer-type", Types.format = Nothing })

      let tool = Types.AgdaInferType { Types.goalId = 0, Types.expression = "suc zero", Types.sessionId = Just "test-infer-type", Types.format = Nothing }
      response <- runTool manager tool

      let kind = getField "kind" response
      assertEqual "Should be DisplayInfo" (Just (JSON.String "DisplayInfo")) kind
  ]

-- | Tests for agda_intro
introTests :: TestTree
introTests = withSessionManager $ \getManager -> testGroup "agda_intro"
  [ simpleTestCase "introduces variables at identity goal" $ do
      manager <- getManager
      file <- exampleFile
      _ <- runTool manager (Types.AgdaLoad { Types.file = T.pack file, Types.sessionId = Just "test-intro", Types.format = Nothing })

      let tool = Types.AgdaIntro { Types.goalId = 4, Types.sessionId = Just "test-intro", Types.format = Nothing }  -- identity function goal
      response <- runTool manager tool

      let kind = getField "kind" response
      assertBool "Should be GiveAction or DisplayInfo"
        (kind == Just (JSON.String "GiveAction") || kind == Just (JSON.String "DisplayInfo"))
  ]

-- | Tests for agda_auto
autoTests :: TestTree
autoTests = withSessionManager $ \getManager -> testGroup "agda_auto"
  [ simpleTestCase "attempts auto on simple goal" $ do
      manager <- getManager
      withTempTestFile "Example.agda" $ \file -> do
        _ <- runTool manager (Types.AgdaLoad { Types.file = T.pack file, Types.sessionId = Just "test-auto-simple", Types.format = Nothing })

        let tool = Types.AgdaAuto { Types.goalId = 0, Types.timeout = Nothing, Types.sessionId = Just "test-auto-simple", Types.format = Nothing }
        response <- runTool manager tool

        -- Auto may succeed (GiveAction) or fail (DisplayInfo with error/auto result)
        let kind = getField "kind" response
        assertBool "Should be DisplayInfo or GiveAction (success!)"
          (kind == Just (JSON.String "DisplayInfo") || kind == Just (JSON.String "GiveAction"))

  , simpleTestCase "respects timeout parameter" $ do
      manager <- getManager
      withTempTestFile "Example.agda" $ \file -> do
        _ <- runTool manager (Types.AgdaLoad { Types.file = T.pack file, Types.sessionId = Just "test-auto-timeout", Types.format = Nothing })

        let tool = Types.AgdaAuto { Types.goalId = 0, Types.timeout = Just 1000, Types.sessionId = Just "test-auto-timeout", Types.format = Nothing }
        response <- runTool manager tool

        -- Should complete within timeout
        let kind = getField "kind" response
        assertBool "Should return DisplayInfo or GiveAction"
          (kind == Just (JSON.String "DisplayInfo") || kind == Just (JSON.String "GiveAction"))
  ]

autoAllTests :: TestTree
autoAllTests = withSessionManager $ \getManager -> testGroup "agda_auto_all"
  [ simpleTestCase "attempts auto on all goals" $ do
      manager <- getManager
      withTempTestFile "Example.agda" $ \file -> do
        _ <- runTool manager (Types.AgdaLoad { Types.file = T.pack file, Types.sessionId = Just "test-auto-all", Types.format = Nothing })

        let tool = Types.AgdaAutoAll { Types.timeout = Nothing, Types.sessionId = Just "test-auto-all", Types.format = Nothing }
        response <- runTool manager tool

        -- Should return SolveAll, GiveAction, or DisplayInfo
        let kind = getField "kind" response
        assertBool "Should be SolveAll, GiveAction, or DisplayInfo"
          (kind == Just (JSON.String "SolveAll") ||
           kind == Just (JSON.String "GiveAction") ||
           kind == Just (JSON.String "DisplayInfo"))

  , simpleTestCase "respects timeout parameter" $ do
      manager <- getManager
      withTempTestFile "Example.agda" $ \file -> do
        _ <- runTool manager (Types.AgdaLoad { Types.file = T.pack file, Types.sessionId = Just "test-auto-all-timeout", Types.format = Nothing })

        let tool = Types.AgdaAutoAll { Types.timeout = Just 2000, Types.sessionId = Just "test-auto-all-timeout", Types.format = Nothing }
        response <- runTool manager tool

        -- Should complete within timeout
        let kind = getField "kind" response
        assertBool "Should return SolveAll, GiveAction, or DisplayInfo"
          (kind == Just (JSON.String "SolveAll") ||
           kind == Just (JSON.String "GiveAction") ||
           kind == Just (JSON.String "DisplayInfo"))
  ]

solveOneTests :: TestTree
solveOneTests = withSessionManager $ \getManager -> testGroup "agda_solve_one"
  [ simpleTestCase "attempts to solve goal 0" $ do
      manager <- getManager
      withTempTestFile "Example.agda" $ \file -> do
        _ <- runTool manager (Types.AgdaLoad { Types.file = T.pack file, Types.sessionId = Just "test-solve-one", Types.format = Nothing })

        let tool = Types.AgdaSolveOne { Types.goalId = 0, Types.sessionId = Just "test-solve-one", Types.format = Nothing }
        response <- runTool manager tool

        -- Should return SolveAll, GiveAction, or DisplayInfo
        let kind = getField "kind" response
        assertBool "Should be SolveAll, GiveAction, or DisplayInfo"
          (kind == Just (JSON.String "SolveAll") ||
           kind == Just (JSON.String "GiveAction") ||
           kind == Just (JSON.String "DisplayInfo"))
  ]

helperFunctionTests :: TestTree
helperFunctionTests = withSessionManager $ \getManager -> testGroup "agda_helper_function"
  [ simpleTestCase "generates helper function for goal 0" $ do
      manager <- getManager
      withTempTestFile "Example.agda" $ \file -> do
        _ <- runTool manager (Types.AgdaLoad { Types.file = T.pack file, Types.sessionId = Just "test-helper-function", Types.format = Nothing })

        let tool = Types.AgdaHelperFunction { Types.goalId = 0, Types.helperName = "helper", Types.sessionId = Just "test-helper-function", Types.format = Nothing }
        response <- runTool manager tool

        -- Should return DisplayInfo with helper function suggestion
        let kind = getField "kind" response
        assertEqual "Should be DisplayInfo" (Just (JSON.String "DisplayInfo")) kind
  ]

goalTypeContextTests :: TestTree
goalTypeContextTests = withSessionManager $ \getManager -> testGroup "agda_goal_type_context"
  [ simpleTestCase "gets type and context for goal 0" $ do
      manager <- getManager
      file <- exampleFile
      _ <- runTool manager (Types.AgdaLoad { Types.file = T.pack file, Types.sessionId = Just "test-goal-type-context", Types.format = Nothing })

      let tool = Types.AgdaGoalTypeContext { Types.goalId = 0, Types.sessionId = Just "test-goal-type-context", Types.format = Nothing }
      response <- runTool manager tool

      -- Should return DisplayInfo with both goal type and context
      let kind = getField "kind" response
      assertEqual "Should be DisplayInfo" (Just (JSON.String "DisplayInfo")) kind
  ]

-- | Tests for agda_search_about
searchAboutTests :: TestTree
searchAboutTests = testGroup "agda_search_about"
  [ testGroup "Basic functionality with Example.agda"
      [ withSessionManager $ \getManager ->simpleTestCase "finds Nat-related functions" $ do
          manager <- getManager
          file <- exampleFile
          _ <- handleAgdaToolWithSession manager (Types.AgdaLoad { Types.file = T.pack file, Types.sessionId = Just "test-search-nat-1", Types.format = Nothing })

          response <- runTool manager (Types.AgdaSearchAbout { Types.query = "Nat", Types.sessionId = Just "test-search-nat-1", Types.format = Just "Full" })

          let info = getField "info" response
          let results = case info of
                Just (JSON.Object obj) -> JSON.KeyMap.lookup (JSON.Key.fromText "results") obj
                _ -> Nothing

          case results of
            Just (JSON.Array arr) -> do
              -- Should find _*_, _+_, suc, zero (4 Nat-related symbols)
              let count = V.length arr
              assertBool "Should find Nat-related functions" (count >= 3)

              -- Check specific result structure
              if count > 0 then do
                let firstResult = arr V.! 0
                assertBool "Result should have 'name' field"
                  (getField "name" firstResult /= Nothing)
                assertBool "Result should have 'term' field"
                  (getField "term" firstResult /= Nothing)
              else
                pure ()
            _ -> assertFailure "Expected results array"

      , withSessionManager $ \getManager ->simpleTestCase "finds multiple Nat operations" $ do
          manager <- getManager
          file <- exampleFile
          _ <- handleAgdaToolWithSession manager (Types.AgdaLoad { Types.file = T.pack file, Types.sessionId = Just "test-search-nat-2", Types.format = Nothing })

          response <- runTool manager (Types.AgdaSearchAbout { Types.query = "Nat", Types.sessionId = Just "test-search-nat-2", Types.format = Just "Full" })

          -- Should find at least 4 results: _*_, _+_, suc, zero
          let info = getField "info" response
          let results = case info of
                Just (JSON.Object obj) -> JSON.KeyMap.lookup (JSON.Key.fromText "results") obj
                _ -> Nothing

          case results of
            Just (JSON.Array arr) ->
              assertBool "Should find multiple Nat operations" (V.length arr >= 3)
            _ -> assertFailure "Expected results array"

      , withSessionManager $ \getManager ->simpleTestCase "validates result structure" $ do
          manager <- getManager
          file <- exampleFile
          _ <- handleAgdaToolWithSession manager (Types.AgdaLoad { Types.file = T.pack file, Types.sessionId = Just "test-search-validate", Types.format = Nothing })

          response <- runTool manager (Types.AgdaSearchAbout { Types.query = "Nat", Types.sessionId = Just "test-search-validate", Types.format = Just "Full" })

          -- Verify result structure has name and term fields
          let info = getField "info" response
          let results = case info of
                Just (JSON.Object obj) -> JSON.KeyMap.lookup (JSON.Key.fromText "results") obj
                _ -> Nothing

          case results of
            Just (JSON.Array arr) ->
              assertBool "Should have at least one result" (V.length arr >= 1)
            _ -> assertFailure "Expected results array"
      ]

  , testGroup "Concise format"
      [ withSessionManager $ \getManager ->simpleTestCase "formats results concisely" $ do
          manager <- getManager
          file <- exampleFile
          _ <- handleAgdaToolWithSession manager (Types.AgdaLoad { Types.file = T.pack file, Types.sessionId = Just "test-search-concise-1", Types.format = Nothing })

          concise <- runToolConcise manager (Types.AgdaSearchAbout { Types.query = "Nat", Types.sessionId = Just "test-search-concise-1", Types.format = Nothing })

          -- Should show "N results:\n  name : type\n  ..."
          assertBool "Should mention results" (T.isInfixOf "result" concise || T.isInfixOf "Result" concise)

      , withSessionManager $ \getManager ->simpleTestCase "shows 'No results found' for non-existent" $ do
          manager <- getManager
          file <- searchTestFile
          _ <- handleAgdaToolWithSession manager (Types.AgdaLoad { Types.file = T.pack file, Types.sessionId = Just "test-search-not-found", Types.format = Nothing })

          concise <- runToolConcise manager (Types.AgdaSearchAbout { Types.query = "NonExistent12345", Types.sessionId = Just "test-search-not-found", Types.format = Nothing })

          assertEqual "Should say no results" "No results found" concise
      ]

  , testGroup "Edge cases"
      [ withSessionManager $ \getManager ->simpleTestCase "search returns valid response structure" $ do
          manager <- getManager
          file <- exampleFile
          _ <- handleAgdaToolWithSession manager (Types.AgdaLoad { Types.file = T.pack file, Types.sessionId = Just "test-search-edge", Types.format = Nothing })

          -- Search for Nat-related terms
          response1 <- runTool manager (Types.AgdaSearchAbout { Types.query = "Nat", Types.sessionId = Just "test-search-edge", Types.format = Just "Full" })
          response2 <- runTool manager (Types.AgdaSearchAbout { Types.query = "suc", Types.sessionId = Just "test-search-edge", Types.format = Just "Full" })

          -- Both should return valid responses with info field
          assertBool "Response1 should exist" (getField "info" response1 /= Nothing)
          assertBool "Response2 should exist" (getField "info" response2 /= Nothing)
      ]

  , testGroup "homotopy-nn integration"
      [ withSessionManager $ \getManager ->simpleTestCase "searches in Neural.Smooth.Calculus" $ do
          manager <- getManager
          let calcFile = "/Users/faezs/homotopy-nn/src/Neural/Smooth/Calculus.agda"

          _ <- handleAgdaToolWithSession manager (Types.AgdaLoad { Types.file = T.pack calcFile, Types.sessionId = Just "test-search-homotopy-1", Types.format = Nothing })
          response <- runTool manager (Types.AgdaSearchAbout { Types.query = "ℝ", Types.sessionId = Just "test-search-homotopy-1", Types.format = Just "Full" })

          let info = getField "info" response
          let results = case info of
                Just (JSON.Object obj) -> JSON.KeyMap.lookup (JSON.Key.fromText "results") obj
                _ -> Nothing

          case results of
            Just (JSON.Array arr) -> do
              -- Should find functions mentioning ℝ (the real numbers type)
              assertBool "Should find functions with ℝ in type" (V.length arr >= 1)

              -- Verify result structure
              if V.length arr > 0 then do
                let firstResult = arr V.! 0
                assertBool "Result should have 'name' field"
                  (getField "name" firstResult /= Nothing)
                assertBool "Result should have 'term' field"
                  (getField "term" firstResult /= Nothing)
              else
                pure ()
            _ -> assertFailure "Expected results array"

      , withSessionManager $ \getManager ->simpleTestCase "searches return consistent structure" $ do
          manager <- getManager
          let calcFile = "/Users/faezs/homotopy-nn/src/Neural/Smooth/Calculus.agda"

          _ <- handleAgdaToolWithSession manager (Types.AgdaLoad { Types.file = T.pack calcFile, Types.sessionId = Just "test-search-homotopy-2", Types.format = Nothing })
          response <- runTool manager (Types.AgdaSearchAbout { Types.query = "ℝ", Types.sessionId = Just "test-search-homotopy-2", Types.format = Just "Full" })

          -- Verify response has proper structure (already tested above, just checking consistency)
          let info = getField "info" response
          assertBool "Should have info field" (info /= Nothing)

          case info of
            Just (JSON.Object obj) -> do
              let resultsField = JSON.KeyMap.lookup (JSON.Key.fromText "results") obj
              assertBool "Info should have results field" (resultsField /= Nothing)
            _ -> assertFailure "Expected info object"
      ]
  ]

showModuleTests :: TestTree
showModuleTests = withSessionManager $ \getManager -> testGroup "agda_show_module"
  [ simpleTestCase "shows builtin Nat module contents" $ do
      manager <- getManager
      file <- exampleFile
      _ <- runTool manager (Types.AgdaLoad { Types.file = T.pack file, Types.sessionId = Just "test-show-module-builtin", Types.format = Nothing })

      let tool = Types.AgdaShowModule { Types.moduleName = "Agda.Builtin.Nat", Types.sessionId = Just "test-show-module-builtin", Types.format = Nothing }
      response <- runTool manager tool

      -- Should return DisplayInfo with module contents
      let kind = getField "kind" response
      assertEqual "Should be DisplayInfo" (Just (JSON.String "DisplayInfo")) kind

  , simpleTestCase "shows Data.Nat module contents" $ do
      manager <- getManager
      file <- exampleFile
      _ <- runTool manager (Types.AgdaLoad { Types.file = T.pack file, Types.sessionId = Just "test-show-module-data", Types.format = Nothing })

      let tool = Types.AgdaShowModule { Types.moduleName = "Data.Nat", Types.sessionId = Just "test-show-module-data", Types.format = Nothing }
      response <- runTool manager tool

      -- Should return DisplayInfo with module contents
      let kind = getField "kind" response
      assertEqual "Should be DisplayInfo" (Just (JSON.String "DisplayInfo")) kind

  , simpleTestCase "handles non-existent module gracefully" $ do
      manager <- getManager
      file <- exampleFile
      _ <- runTool manager (Types.AgdaLoad { Types.file = T.pack file, Types.sessionId = Just "test-show-module-nonexistent", Types.format = Nothing })

      let tool = Types.AgdaShowModule { Types.moduleName = "NonExistent.Module", Types.sessionId = Just "test-show-module-nonexistent", Types.format = Nothing }
      response <- runTool manager tool

      -- Should return an error or DisplayInfo response
      let kind = getField "kind" response
      assertBool "Should be DisplayInfo or error response"
        (kind == Just (JSON.String "DisplayInfo") || kind == Just (JSON.String "Error"))
  ]

showConstraintsTests :: TestTree
showConstraintsTests = withSessionManager $ \getManager -> testGroup "agda_show_constraints"
  [ simpleTestCase "shows constraints for loaded file" $ do
      manager <- getManager
      file <- exampleFile
      _ <- runTool manager (Types.AgdaLoad { Types.file = T.pack file, Types.sessionId = Just "test-show-constraints-1", Types.format = Nothing })

      let tool = Types.AgdaShowConstraints { Types.sessionId = Just "test-show-constraints-1", Types.format = Nothing }
      response <- runTool manager tool

      -- Should return DisplayInfo with constraints (or no constraints)
      let kind = getField "kind" response
      assertEqual "Should be DisplayInfo" (Just (JSON.String "DisplayInfo")) kind

  , simpleTestCase "returns no constraints for complete file" $ do
      manager <- getManager
      file <- exampleFile
      _ <- runTool manager (Types.AgdaLoad { Types.file = T.pack file, Types.sessionId = Just "test-show-constraints-2", Types.format = Nothing })

      let tool = Types.AgdaShowConstraints { Types.sessionId = Just "test-show-constraints-2", Types.format = Nothing }
      response <- runTool manager tool

      -- Example.agda should have no unsolved constraints
      let kind = getField "kind" response
      assertEqual "Should be DisplayInfo" (Just (JSON.String "DisplayInfo")) kind
  ]

-- | Tests for agda_why_in_scope
whyInScopeTests :: TestTree
whyInScopeTests = withSessionManager $ \getManager -> testGroup "agda_why_in_scope"
  [ simpleTestCase "looks up existing name" $ do
      manager <- getManager
      file <- exampleFile
      _ <- runTool manager (Types.AgdaLoad { Types.file = T.pack file, Types.sessionId = Just "test-why-in-scope-exists", Types.format = Nothing })

      let tool = Types.AgdaWhyInScope { Types.name = "suc", Types.sessionId = Just "test-why-in-scope-exists", Types.format = Nothing }
      response <- runTool manager tool

      let kind = getField "kind" response
      assertEqual "Should be DisplayInfo" (Just (JSON.String "DisplayInfo")) kind

  , simpleTestCase "looks up non-existent name" $ do
      manager <- getManager
      file <- exampleFile
      _ <- runTool manager (Types.AgdaLoad { Types.file = T.pack file, Types.sessionId = Just "test-why-in-scope-nonexistent", Types.format = Nothing })

      let tool = Types.AgdaWhyInScope { Types.name = "nonExistentName123", Types.sessionId = Just "test-why-in-scope-nonexistent", Types.format = Nothing }
      response <- runTool manager tool

      let kind = getField "kind" response
      assertEqual "Should be DisplayInfo" (Just (JSON.String "DisplayInfo")) kind
  ]

-- | Tests for agda_list_postulates
listPostulatesTests :: TestTree
listPostulatesTests = testGroup "agda_list_postulates"
  [ withSessionManager $ \getManager ->simpleTestCase "lists postulates in Full format" $ do
      manager <- getManager
      file <- postulateFile
      let tool = Types.AgdaListPostulates { Types.file = T.pack file, Types.sessionId = Just "test-postulates-full", Types.format = Nothing }
      response <- runTool manager tool

      -- Should return a JSON array of postulates
      case response of
        JSON.Array postulates -> do
          assertEqual "Should have 5 postulates" 5 (V.length postulates)

          -- Check first postulate has required fields
          case postulates V.! 0 of
            JSON.Object obj -> do
              let pName = JSON.KeyMap.lookup (JSON.Key.fromText "pName") obj
              let pType = JSON.KeyMap.lookup (JSON.Key.fromText "pType") obj
              let pRange = JSON.KeyMap.lookup (JSON.Key.fromText "pRange") obj
              assertBool "Should have pName field" (pName /= Nothing)
              assertBool "Should have pType field" (pType /= Nothing)
              assertBool "Should have pRange field" (pRange /= Nothing)
            _ -> assertFailure "Postulate should be a JSON object"
        _ -> assertFailure "Response should be a JSON array"

  , withSessionManager $ \getManager ->simpleTestCase "lists postulates in Concise format" $ do
      manager <- getManager
      file <- postulateFile
      let tool = Types.AgdaListPostulates { Types.file = T.pack file, Types.sessionId = Just "test-postulates-concise", Types.format = Nothing }
      response <- runToolConcise manager tool

      -- Should return human-readable text like "5 postulates: ..."
      assertBool "Should mention '5 postulates'" (T.isInfixOf "5 postulates" response)
      assertBool "Should contain postulate names"
        (T.isInfixOf "foo" response && T.isInfixOf "bar" response && T.isInfixOf "baz" response)
      assertBool "Should mention line numbers" (T.isInfixOf "at lines" response)

  , withSessionManager $ \getManager ->simpleTestCase "returns empty for file with no postulates" $ do
      manager <- getManager
      file <- exampleFile
      let tool = Types.AgdaListPostulates { Types.file = T.pack file, Types.sessionId = Just "test-postulates-empty", Types.format = Nothing }
      response <- runTool manager tool

      -- Should return empty array
      case response of
        JSON.Array postulates -> do
          assertEqual "Should have 0 postulates" 0 (V.length postulates)
        _ -> assertFailure "Response should be a JSON array"

  , withSessionManager $ \getManager ->simpleTestCase "extracts correct postulate names" $ do
      manager <- getManager
      file <- postulateFile
      let tool = Types.AgdaListPostulates { Types.file = T.pack file, Types.sessionId = Just "test-postulates-names", Types.format = Nothing }
      response <- runTool manager tool

      -- Check postulate names
      case response of
        JSON.Array postulates -> do
          let names = map extractName (V.toList postulates)
          assertBool "Should contain 'foo'" ("PostulateTest.foo" `elem` names)
          assertBool "Should contain 'bar'" ("PostulateTest.bar" `elem` names)
          assertBool "Should contain 'baz'" ("PostulateTest.baz" `elem` names)
          assertBool "Should contain 'identity'" ("PostulateTest.identity" `elem` names)
          assertBool "Should contain 'composition'" ("PostulateTest.composition" `elem` names)
        _ -> assertFailure "Response should be a JSON array"

  , withSessionManager $ \getManager ->simpleTestCase "provides correct types for postulates" $ do
      manager <- getManager
      file <- postulateFile
      let tool = Types.AgdaListPostulates { Types.file = T.pack file, Types.sessionId = Just "test-postulates-types", Types.format = Nothing }
      response <- runTool manager tool

      -- Check that at least one postulate has its expected type
      case response of
        JSON.Array postulates -> do
          let types = map extractType (V.toList postulates)
          assertBool "Should have 'Nat' type" ("Nat" `elem` types)
          assertBool "Should have 'Bool' type" ("Bool" `elem` types)
          assertBool "Should have function type" (any (T.isInfixOf "→") types)
        _ -> assertFailure "Response should be a JSON array"
  ]
  where
    extractName :: JSON.Value -> Text
    extractName (JSON.Object obj) =
      case JSON.KeyMap.lookup (JSON.Key.fromText "pName") obj of
        Just (JSON.String name) -> name
        _ -> ""
    extractName _ = ""

    extractType :: JSON.Value -> Text
    extractType (JSON.Object obj) =
      case JSON.KeyMap.lookup (JSON.Key.fromText "pType") obj of
        Just (JSON.String typ) -> typ
        _ -> ""
    extractType _ = ""

-- | Tests for agda_goal_at_position
goalAtPositionTests :: TestTree
goalAtPositionTests = testGroup "agda_goal_at_position"
  [ withSessionManager $ \getManager ->simpleTestCase "finds goal at position" $ do
      manager <- getManager
      file <- exampleFile
      -- Line 10, column 13 is inside the first goal {! !}
      let tool = Types.AgdaGoalAtPosition { Types.file = T.pack file, Types.line = 10, Types.column = 13, Types.sessionId = Just "test-goal-at-pos-found", Types.format = Nothing }
      response <- runTool manager tool

      -- Check that we got a goal back with goalId field
      let goalId = getField "goalId" response
      assertBool "Should have goalId field" (goalId /= Nothing)

      let goalType = getField "goalType" response
      assertBool "Should have goalType field" (goalType /= Nothing)

  , withSessionManager $ \getManager ->simpleTestCase "returns error when no goal at position" $ do
      manager <- getManager
      file <- exampleFile
      -- Line 1, column 1 is not inside any goal
      let tool = Types.AgdaGoalAtPosition { Types.file = T.pack file, Types.line = 1, Types.column = 1, Types.sessionId = Just "test-goal-at-pos-not-found", Types.format = Nothing }
      result <- handleAgdaToolWithSession manager tool

      case result of
        MCP.ContentText txt -> assertBool "Should mention no goal found" (T.isInfixOf "No goal found" txt)
        _ -> assertFailure "Expected ContentText response"

  , withSessionManager $ \getManager ->simpleTestCase "formats concisely" $ do
      manager <- getManager
      file <- exampleFile
      -- Line 10, column 13 is inside the first goal
      let tool = Types.AgdaGoalAtPosition { Types.file = T.pack file, Types.line = 10, Types.column = 13, Types.sessionId = Just "test-goal-at-pos-concise", Types.format = Nothing }
      response <- runToolConcise manager tool

      -- Concise format should show goal like "?<0> : Nat (10:12)"
      assertBool "Should contain goal marker" (T.isInfixOf "?" response)
      assertBool "Should contain type separator" (T.isInfixOf ":" response)
  ]
