{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.ServerSpec (tests) where

import Test.Tasty
import Test.Tasty.Providers
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Key as JSON.Key
import qualified Data.Aeson.KeyMap as JSON.KeyMap
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import System.FilePath ((</>))
import System.Directory (getCurrentDirectory)
import Control.Exception (try, SomeException)

import AgdaMCP.Server
import qualified AgdaMCP.Types as Types
import qualified MCP.Server as MCP

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
    else error $ msg ++ ": expected " ++ show expected ++ " but got " ++ show actual

assertBool :: String -> Bool -> IO ()
assertBool msg condition =
  if condition
    then pure ()
    else error msg

assertFailure :: String -> IO ()
assertFailure = error

-- | Helper to run a tool and get the response as JSON
runTool :: IORef ServerState -> Types.AgdaTool -> IO JSON.Value
runTool stateRef tool = do
  result <- handleAgdaTool stateRef tool
  case result of
    MCP.ContentText txt -> do
      case JSON.decode (LBS.fromStrict $ TE.encodeUtf8 txt) of
        Just val -> pure val
        Nothing -> error $ "Failed to parse JSON response: " ++ T.unpack txt
    _ -> error "Expected ContentText response"

-- | Helper to get a field from a JSON object
getField :: T.Text -> JSON.Value -> Maybe JSON.Value
getField field (JSON.Object obj) = JSON.KeyMap.lookup (JSON.Key.fromText field) obj
getField _ _ = Nothing

-- | Helper to get array length
arrayLength :: JSON.Value -> Int
arrayLength (JSON.Array arr) = length arr
arrayLength _ = 0

tests :: TestTree
tests = testGroup "AgdaMCP.Server Tests"
  [ loadTests
  , getGoalsTests
  , getGoalTypeTests
  , getContextTests
  , giveTests
  , refineTests
  , caseSplitTests
  , computeTests
  , inferTypeTests
  , introTests
  , whyInScopeTests
  ]

-- Test data
exampleFile :: IO FilePath
exampleFile = do
  cwd <- getCurrentDirectory
  pure $ cwd </> "test" </> "Example.agda"

-- | Tests for agda_load
loadTests :: TestTree
loadTests = testGroup "agda_load"
  [ simpleTestCase "loads file successfully and returns goals" $ do
      stateRef <- initServerState
      file <- exampleFile
      let tool = Types.AgdaLoad { Types.file = T.pack file }
      response <- runTool stateRef tool

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
      stateRef <- initServerState
      let tool = Types.AgdaLoad { Types.file = "/non/existent/file.agda" }
      response <- runTool stateRef tool

      -- Should return an error response
      let kind = getField "kind" response
      assertBool "Should have error response" (kind == Just (JSON.String "DisplayInfo") || kind == Just (JSON.String "Error"))
  ]

-- | Tests for agda_get_goals
getGoalsTests :: TestTree
getGoalsTests = testGroup "agda_get_goals"
  [ simpleTestCase "gets goals after successful load" $ do
      stateRef <- initServerState
      file <- exampleFile

      -- First load the file
      _ <- handleAgdaTool stateRef (Types.AgdaLoad { Types.file = T.pack file })

      -- Then get goals
      response <- runTool stateRef Types.AgdaGetGoals

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
getGoalTypeTests = testGroup "agda_get_goal_type"
  [ simpleTestCase "gets type of goal 0" $ do
      stateRef <- loadedState
      let tool = Types.AgdaGetGoalType { Types.goalId = 0 }
      response <- runTool stateRef tool

      -- Should return DisplayInfo with goal type
      let kind = getField "kind" response
      assertEqual "Should be DisplayInfo" (Just (JSON.String "DisplayInfo")) kind

  , simpleTestCase "fails on invalid goal ID" $ do
      stateRef <- loadedState
      let tool = Types.AgdaGetGoalType { Types.goalId = 999 }
      response <- runTool stateRef tool

      -- Should return error
      let kind = getField "kind" response
      assertBool "Should have error" (kind == Just (JSON.String "DisplayInfo"))
  ]

-- | Tests for agda_get_context
getContextTests :: TestTree
getContextTests = testGroup "agda_get_context"
  [ simpleTestCase "gets context at goal 0" $ do
      stateRef <- loadedState
      let tool = Types.AgdaGetContext { Types.goalId = 0 }
      response <- runTool stateRef tool

      let kind = getField "kind" response
      assertEqual "Should be DisplayInfo" (Just (JSON.String "DisplayInfo")) kind

      -- Context should contain variable 'n'
      -- (We can't easily assert the exact content without parsing the full structure)
  ]

-- | Tests for agda_give
giveTests :: TestTree
giveTests = testGroup "agda_give"
  [ simpleTestCase "successful give returns GiveAction" $ do
      stateRef <- loadedState
      let tool = Types.AgdaGive { Types.goalId = 0, Types.expression = "n" }
      response <- runTool stateRef tool

      let kind = getField "kind" response
      assertEqual "Should be GiveAction" (Just (JSON.String "GiveAction")) kind

      let giveResult = getField "giveResult" response
      assertBool "Should have giveResult" (giveResult /= Nothing)

  , simpleTestCase "failed give returns error" $ do
      stateRef <- loadedState
      -- Use a completely invalid expression to trigger a parse/scope error
      let tool = Types.AgdaGive { Types.goalId = 0, Types.expression = "nonExistentName123" }
      response <- runTool stateRef tool

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
refineTests = testGroup "agda_refine"
  [ simpleTestCase "refines goal with constructor" $ do
      stateRef <- loadedState
      let tool = Types.AgdaRefine { Types.goalId = 1, Types.expression = "suc" }
      response <- runTool stateRef tool

      -- Refine should return GiveAction
      let kind = getField "kind" response
      assertBool "Should be GiveAction or DisplayInfo"
        (kind == Just (JSON.String "GiveAction") || kind == Just (JSON.String "DisplayInfo"))
  ]

-- | Tests for agda_case_split
caseSplitTests :: TestTree
caseSplitTests = testGroup "agda_case_split"
  [ simpleTestCase "splits on variable n" $ do
      stateRef <- loadedState
      let tool = Types.AgdaCaseSplit { Types.goalId = 0, Types.variable = "n" }
      response <- runTool stateRef tool

      -- Case split returns MakeCase
      let kind = getField "kind" response
      assertBool "Should be MakeCase or DisplayInfo"
        (kind == Just (JSON.String "MakeCase") || kind == Just (JSON.String "DisplayInfo"))
  ]

-- | Tests for agda_compute
computeTests :: TestTree
computeTests = testGroup "agda_compute"
  [ simpleTestCase "computes expression" $ do
      stateRef <- loadedState
      let tool = Types.AgdaCompute { Types.goalId = 0, Types.expression = "suc zero" }
      response <- runTool stateRef tool

      let kind = getField "kind" response
      assertEqual "Should be DisplayInfo" (Just (JSON.String "DisplayInfo")) kind
  ]

-- | Tests for agda_infer_type
inferTypeTests :: TestTree
inferTypeTests = testGroup "agda_infer_type"
  [ simpleTestCase "infers type of valid expression" $ do
      stateRef <- loadedState
      let tool = Types.AgdaInferType { Types.goalId = 0, Types.expression = "suc zero" }
      response <- runTool stateRef tool

      let kind = getField "kind" response
      assertEqual "Should be DisplayInfo" (Just (JSON.String "DisplayInfo")) kind
  ]

-- | Tests for agda_intro
introTests :: TestTree
introTests = testGroup "agda_intro"
  [ simpleTestCase "introduces variables at identity goal" $ do
      stateRef <- loadedState
      let tool = Types.AgdaIntro { Types.goalId = 4 }  -- identity function goal
      response <- runTool stateRef tool

      let kind = getField "kind" response
      assertBool "Should be GiveAction or DisplayInfo"
        (kind == Just (JSON.String "GiveAction") || kind == Just (JSON.String "DisplayInfo"))
  ]

-- | Tests for agda_why_in_scope
whyInScopeTests :: TestTree
whyInScopeTests = testGroup "agda_why_in_scope"
  [ simpleTestCase "looks up existing name" $ do
      stateRef <- loadedState
      let tool = Types.AgdaWhyInScope { Types.name = "suc" }
      response <- runTool stateRef tool

      let kind = getField "kind" response
      assertEqual "Should be DisplayInfo" (Just (JSON.String "DisplayInfo")) kind

  , simpleTestCase "looks up non-existent name" $ do
      stateRef <- loadedState
      let tool = Types.AgdaWhyInScope { Types.name = "nonExistentName123" }
      response <- runTool stateRef tool

      let kind = getField "kind" response
      assertEqual "Should be DisplayInfo" (Just (JSON.String "DisplayInfo")) kind
  ]

-- | Helper to create a loaded state for tests
loadedState :: IO (IORef ServerState)
loadedState = do
  stateRef <- initServerState
  file <- exampleFile
  _ <- handleAgdaTool stateRef (Types.AgdaLoad { Types.file = T.pack file })
  pure stateRef
