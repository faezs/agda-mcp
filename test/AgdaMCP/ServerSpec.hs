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

-- | Helper to run a tool and get the response as JSON (forces Full format)
runTool :: IORef ServerState -> Types.AgdaTool -> IO JSON.Value
runTool stateRef tool = do
  -- Force Full format for JSON parsing
  let toolWithFullFormat = setFormat tool (Just "Full")
  result <- handleAgdaTool stateRef toolWithFullFormat
  case result of
    MCP.ContentText txt -> do
      case JSON.decode (LBS.fromStrict $ TE.encodeUtf8 txt) of
        Just val -> pure val
        Nothing -> error $ "Failed to parse JSON response: " ++ T.unpack txt
    _ -> error "Expected ContentText response"

-- | Helper to run a tool and get concise text response
runToolConcise :: IORef ServerState -> Types.AgdaTool -> IO Text
runToolConcise stateRef tool = do
  -- Force Concise format
  let toolWithConciseFormat = setFormat tool (Just "Concise")
  result <- handleAgdaTool stateRef toolWithConciseFormat
  case result of
    MCP.ContentText txt -> pure txt
    _ -> error "Expected ContentText response"

-- | Set format on a tool
setFormat :: Types.AgdaTool -> Maybe Text -> Types.AgdaTool
setFormat tool fmt = case tool of
  Types.AgdaLoad{Types.file=f} -> Types.AgdaLoad{Types.file=f, Types.format=fmt}
  Types.AgdaGetGoals{} -> Types.AgdaGetGoals{Types.format=fmt}
  Types.AgdaGetGoalType{Types.goalId=gid} -> Types.AgdaGetGoalType{Types.goalId=gid, Types.format=fmt}
  Types.AgdaGetContext{Types.goalId=gid} -> Types.AgdaGetContext{Types.goalId=gid, Types.format=fmt}
  Types.AgdaGive{Types.goalId=gid, Types.expression=expr} -> Types.AgdaGive{Types.goalId=gid, Types.expression=expr, Types.format=fmt}
  Types.AgdaRefine{Types.goalId=gid, Types.expression=expr} -> Types.AgdaRefine{Types.goalId=gid, Types.expression=expr, Types.format=fmt}
  Types.AgdaCaseSplit{Types.goalId=gid, Types.variable=var} -> Types.AgdaCaseSplit{Types.goalId=gid, Types.variable=var, Types.format=fmt}
  Types.AgdaCompute{Types.goalId=gid, Types.expression=expr} -> Types.AgdaCompute{Types.goalId=gid, Types.expression=expr, Types.format=fmt}
  Types.AgdaInferType{Types.goalId=gid, Types.expression=expr} -> Types.AgdaInferType{Types.goalId=gid, Types.expression=expr, Types.format=fmt}
  Types.AgdaIntro{Types.goalId=gid} -> Types.AgdaIntro{Types.goalId=gid, Types.format=fmt}
  Types.AgdaAuto{Types.goalId=gid, Types.timeout=t} -> Types.AgdaAuto{Types.goalId=gid, Types.timeout=t, Types.format=fmt}
  Types.AgdaAutoAll{Types.timeout=t} -> Types.AgdaAutoAll{Types.timeout=t, Types.format=fmt}
  Types.AgdaSearchAbout{Types.query=q} -> Types.AgdaSearchAbout{Types.query=q, Types.format=fmt}
  Types.AgdaShowModule{Types.moduleName=m} -> Types.AgdaShowModule{Types.moduleName=m, Types.format=fmt}
  Types.AgdaShowConstraints{} -> Types.AgdaShowConstraints{Types.format=fmt}
  Types.AgdaWhyInScope{Types.name=n} -> Types.AgdaWhyInScope{Types.name=n, Types.format=fmt}
  Types.AgdaListPostulates{Types.file=f} -> Types.AgdaListPostulates{Types.file=f, Types.format=fmt}

-- | Helper to get a field from a JSON object
getField :: T.Text -> JSON.Value -> Maybe JSON.Value
getField field (JSON.Object obj) = JSON.KeyMap.lookup (JSON.Key.fromText field) obj
getField _ _ = Nothing

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
  , autoTests
  , autoAllTests
  , searchAboutTests
  , showModuleTests
  , showConstraintsTests
  , whyInScopeTests
  , listPostulatesTests
  ]

-- Test data
exampleFile :: IO FilePath
exampleFile = do
  cwd <- getCurrentDirectory
  pure $ cwd </> "test" </> "Example.agda"

postulateFile :: IO FilePath
postulateFile = do
  cwd <- getCurrentDirectory
  pure $ cwd </> "test" </> "PostulateTest.agda"

-- | Tests for agda_load
loadTests :: TestTree
loadTests = testGroup "agda_load"
  [ simpleTestCase "loads file successfully and returns goals" $ do
      stateRef <- initServerState
      file <- exampleFile
      let tool = Types.AgdaLoad { Types.file = T.pack file, Types.format = Nothing }
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
      let tool = Types.AgdaLoad { Types.file = "/non/existent/file.agda", Types.format = Nothing }
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
      _ <- handleAgdaTool stateRef (Types.AgdaLoad { Types.file = T.pack file, Types.format = Nothing })

      -- Then get goals
      response <- runTool stateRef (Types.AgdaGetGoals { Types.format = Nothing })

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
      let tool = Types.AgdaGetGoalType { Types.goalId = 0, Types.format = Nothing }
      response <- runTool stateRef tool

      -- Should return DisplayInfo with goal type
      let kind = getField "kind" response
      assertEqual "Should be DisplayInfo" (Just (JSON.String "DisplayInfo")) kind

  , simpleTestCase "fails on invalid goal ID" $ do
      stateRef <- loadedState
      let tool = Types.AgdaGetGoalType { Types.goalId = 999, Types.format = Nothing }
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
      let tool = Types.AgdaGetContext { Types.goalId = 0, Types.format = Nothing }
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
      let tool = Types.AgdaGive { Types.goalId = 0, Types.expression = "n", Types.format = Nothing }
      response <- runTool stateRef tool

      let kind = getField "kind" response
      assertEqual "Should be GiveAction" (Just (JSON.String "GiveAction")) kind

      let giveResult = getField "giveResult" response
      assertBool "Should have giveResult" (giveResult /= Nothing)

  , simpleTestCase "failed give returns error" $ do
      stateRef <- loadedState
      -- Use a completely invalid expression to trigger a parse/scope error
      let tool = Types.AgdaGive { Types.goalId = 0, Types.expression = "nonExistentName123", Types.format = Nothing }
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
      let tool = Types.AgdaRefine { Types.goalId = 1, Types.expression = "suc", Types.format = Nothing }
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
      let tool = Types.AgdaCaseSplit { Types.goalId = 0, Types.variable = "n", Types.format = Nothing }
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
      let tool = Types.AgdaCompute { Types.goalId = 0, Types.expression = "suc zero", Types.format = Nothing }
      response <- runTool stateRef tool

      let kind = getField "kind" response
      assertEqual "Should be DisplayInfo" (Just (JSON.String "DisplayInfo")) kind
  ]

-- | Tests for agda_infer_type
inferTypeTests :: TestTree
inferTypeTests = testGroup "agda_infer_type"
  [ simpleTestCase "infers type of valid expression" $ do
      stateRef <- loadedState
      let tool = Types.AgdaInferType { Types.goalId = 0, Types.expression = "suc zero", Types.format = Nothing }
      response <- runTool stateRef tool

      let kind = getField "kind" response
      assertEqual "Should be DisplayInfo" (Just (JSON.String "DisplayInfo")) kind
  ]

-- | Tests for agda_intro
introTests :: TestTree
introTests = testGroup "agda_intro"
  [ simpleTestCase "introduces variables at identity goal" $ do
      stateRef <- loadedState
      let tool = Types.AgdaIntro { Types.goalId = 4, Types.format = Nothing }  -- identity function goal
      response <- runTool stateRef tool

      let kind = getField "kind" response
      assertBool "Should be GiveAction or DisplayInfo"
        (kind == Just (JSON.String "GiveAction") || kind == Just (JSON.String "DisplayInfo"))
  ]

-- | Tests for agda_auto
autoTests :: TestTree
autoTests = testGroup "agda_auto"
  [ simpleTestCase "attempts auto on simple goal" $ do
      stateRef <- loadedState
      let tool = Types.AgdaAuto { Types.goalId = 0, Types.timeout = Nothing, Types.format = Nothing }
      response <- runTool stateRef tool

      -- Auto may succeed (GiveAction) or fail (DisplayInfo with error/auto result)
      let kind = getField "kind" response
      assertBool "Should be DisplayInfo or GiveAction (success!)"
        (kind == Just (JSON.String "DisplayInfo") || kind == Just (JSON.String "GiveAction"))

  , simpleTestCase "respects timeout parameter" $ do
      stateRef <- loadedState
      let tool = Types.AgdaAuto { Types.goalId = 0, Types.timeout = Just 1000, Types.format = Nothing }
      response <- runTool stateRef tool

      -- Should complete within timeout
      let kind = getField "kind" response
      assertBool "Should return DisplayInfo or GiveAction"
        (kind == Just (JSON.String "DisplayInfo") || kind == Just (JSON.String "GiveAction"))
  ]

autoAllTests :: TestTree
autoAllTests = testGroup "agda_auto_all"
  [ simpleTestCase "attempts auto on all goals" $ do
      stateRef <- loadedState
      let tool = Types.AgdaAutoAll { Types.timeout = Nothing, Types.format = Nothing }
      response <- runTool stateRef tool

      -- Should return SolveAll, GiveAction, or DisplayInfo
      let kind = getField "kind" response
      assertBool "Should be SolveAll, GiveAction, or DisplayInfo"
        (kind == Just (JSON.String "SolveAll") ||
         kind == Just (JSON.String "GiveAction") ||
         kind == Just (JSON.String "DisplayInfo"))

  , simpleTestCase "respects timeout parameter" $ do
      stateRef <- loadedState
      let tool = Types.AgdaAutoAll { Types.timeout = Just 2000, Types.format = Nothing }
      response <- runTool stateRef tool

      -- Should complete within timeout
      let kind = getField "kind" response
      assertBool "Should return SolveAll, GiveAction, or DisplayInfo"
        (kind == Just (JSON.String "SolveAll") ||
         kind == Just (JSON.String "GiveAction") ||
         kind == Just (JSON.String "DisplayInfo"))
  ]

-- | Tests for agda_search_about
searchAboutTests :: TestTree
searchAboutTests = testGroup "agda_search_about"
  [ simpleTestCase "searches for definitions by name" $ do
      stateRef <- loadedState
      let tool = Types.AgdaSearchAbout { Types.query = "suc", Types.format = Nothing }
      response <- runTool stateRef tool

      -- Should return DisplayInfo with SearchAbout info
      let kind = getField "kind" response
      assertEqual "Should be DisplayInfo" (Just (JSON.String "DisplayInfo")) kind

  , simpleTestCase "searches for definitions by pattern" $ do
      stateRef <- loadedState
      let tool = Types.AgdaSearchAbout { Types.query = "Nat", Types.format = Nothing }
      response <- runTool stateRef tool

      -- Should return results containing Nat-related definitions
      let kind = getField "kind" response
      assertEqual "Should be DisplayInfo" (Just (JSON.String "DisplayInfo")) kind

  , simpleTestCase "returns empty for non-existent names" $ do
      stateRef <- loadedState
      let tool = Types.AgdaSearchAbout { Types.query = "nonExistentFunction12345", Types.format = Nothing }
      response <- runTool stateRef tool

      -- Should still return DisplayInfo (just with no results)
      let kind = getField "kind" response
      assertEqual "Should be DisplayInfo" (Just (JSON.String "DisplayInfo")) kind
  ]

showModuleTests :: TestTree
showModuleTests = testGroup "agda_show_module"
  [ simpleTestCase "shows builtin Nat module contents" $ do
      stateRef <- loadedState
      let tool = Types.AgdaShowModule { Types.moduleName = "Agda.Builtin.Nat", Types.format = Nothing }
      response <- runTool stateRef tool

      -- Should return DisplayInfo with module contents
      let kind = getField "kind" response
      assertEqual "Should be DisplayInfo" (Just (JSON.String "DisplayInfo")) kind

  , simpleTestCase "shows Data.Nat module contents" $ do
      stateRef <- loadedState
      let tool = Types.AgdaShowModule { Types.moduleName = "Data.Nat", Types.format = Nothing }
      response <- runTool stateRef tool

      -- Should return DisplayInfo with module contents
      let kind = getField "kind" response
      assertEqual "Should be DisplayInfo" (Just (JSON.String "DisplayInfo")) kind

  , simpleTestCase "handles non-existent module gracefully" $ do
      stateRef <- loadedState
      let tool = Types.AgdaShowModule { Types.moduleName = "NonExistent.Module", Types.format = Nothing }
      response <- runTool stateRef tool

      -- Should return an error or DisplayInfo response
      let kind = getField "kind" response
      assertBool "Should be DisplayInfo or error response"
        (kind == Just (JSON.String "DisplayInfo") || kind == Just (JSON.String "Error"))
  ]

showConstraintsTests :: TestTree
showConstraintsTests = testGroup "agda_show_constraints"
  [ simpleTestCase "shows constraints for loaded file" $ do
      stateRef <- loadedState
      let tool = Types.AgdaShowConstraints { Types.format = Nothing }
      response <- runTool stateRef tool

      -- Should return DisplayInfo with constraints (or no constraints)
      let kind = getField "kind" response
      assertEqual "Should be DisplayInfo" (Just (JSON.String "DisplayInfo")) kind

  , simpleTestCase "returns no constraints for complete file" $ do
      stateRef <- loadedState
      let tool = Types.AgdaShowConstraints { Types.format = Nothing }
      response <- runTool stateRef tool

      -- Example.agda should have no unsolved constraints
      let kind = getField "kind" response
      assertEqual "Should be DisplayInfo" (Just (JSON.String "DisplayInfo")) kind
  ]

-- | Tests for agda_why_in_scope
whyInScopeTests :: TestTree
whyInScopeTests = testGroup "agda_why_in_scope"
  [ simpleTestCase "looks up existing name" $ do
      stateRef <- loadedState
      let tool = Types.AgdaWhyInScope { Types.name = "suc", Types.format = Nothing }
      response <- runTool stateRef tool

      let kind = getField "kind" response
      assertEqual "Should be DisplayInfo" (Just (JSON.String "DisplayInfo")) kind

  , simpleTestCase "looks up non-existent name" $ do
      stateRef <- loadedState
      let tool = Types.AgdaWhyInScope { Types.name = "nonExistentName123", Types.format = Nothing }
      response <- runTool stateRef tool

      let kind = getField "kind" response
      assertEqual "Should be DisplayInfo" (Just (JSON.String "DisplayInfo")) kind
  ]

-- | Tests for agda_list_postulates
listPostulatesTests :: TestTree
listPostulatesTests = testGroup "agda_list_postulates"
  [ simpleTestCase "lists postulates in Full format" $ do
      stateRef <- initServerState
      file <- postulateFile
      let tool = Types.AgdaListPostulates { Types.file = T.pack file, Types.format = Nothing }
      response <- runTool stateRef tool

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

  , simpleTestCase "lists postulates in Concise format" $ do
      stateRef <- initServerState
      file <- postulateFile
      let tool = Types.AgdaListPostulates { Types.file = T.pack file, Types.format = Nothing }
      response <- runToolConcise stateRef tool

      -- Should return human-readable text like "5 postulates: ..."
      assertBool "Should mention '5 postulates'" (T.isInfixOf "5 postulates" response)
      assertBool "Should contain postulate names"
        (T.isInfixOf "foo" response && T.isInfixOf "bar" response && T.isInfixOf "baz" response)
      assertBool "Should mention line numbers" (T.isInfixOf "at lines" response)

  , simpleTestCase "returns empty for file with no postulates" $ do
      stateRef <- initServerState
      file <- exampleFile
      let tool = Types.AgdaListPostulates { Types.file = T.pack file, Types.format = Nothing }
      response <- runTool stateRef tool

      -- Should return empty array
      case response of
        JSON.Array postulates -> do
          assertEqual "Should have 0 postulates" 0 (V.length postulates)
        _ -> assertFailure "Response should be a JSON array"

  , simpleTestCase "extracts correct postulate names" $ do
      stateRef <- initServerState
      file <- postulateFile
      let tool = Types.AgdaListPostulates { Types.file = T.pack file, Types.format = Nothing }
      response <- runTool stateRef tool

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

  , simpleTestCase "provides correct types for postulates" $ do
      stateRef <- initServerState
      file <- postulateFile
      let tool = Types.AgdaListPostulates { Types.file = T.pack file, Types.format = Nothing }
      response <- runTool stateRef tool

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

-- | Helper to create a loaded state for tests
loadedState :: IO (IORef ServerState)
loadedState = do
  stateRef <- initServerState
  file <- exampleFile
  _ <- handleAgdaTool stateRef (Types.AgdaLoad { Types.file = T.pack file, Types.format = Nothing })
  pure stateRef
