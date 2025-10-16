{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.MultiAgentSpec (tests) where

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
import Control.Exception (try, SomeException, bracket)
import Control.Concurrent (killThread)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)

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

-- | Cleanup function to kill REPL thread and free resources
cleanupServerState :: IORef ServerState -> IO ()
cleanupServerState stateRef = do
  state <- readIORef stateRef
  case replThreadId state of
    Just tid -> killThread tid
    Nothing -> return ()

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

-- | Set format on a tool (preserves sessionId) - simplified for GetGoals only
setFormat :: Types.AgdaTool -> Maybe Text -> Types.AgdaTool
setFormat (Types.AgdaGetGoals{Types.sessionId=sid}) fmt =
  Types.AgdaGetGoals{Types.sessionId=sid, Types.format=fmt}
setFormat tool _ = tool  -- For other tools, just return as-is

-- ============================================================================
-- Multi-Agent Session Isolation Tests
-- ============================================================================

tests :: TestTree
tests = testGroup "Multi-Agent Session Isolation"
  [ simpleTestCase "multiple independent states maintain isolation" $ do
      hPutStrLn stderr "\n=== Starting Multi-Agent Test ==="

      -- Setup: Get file paths
      cwd <- getCurrentDirectory
      let examplePath = cwd </> "test" </> "Example.agda"
      let postulateFile = cwd </> "test" </> "PostulateTest.agda"

      hPutStrLn stderr $ "Example path: " ++ examplePath
      hPutStrLn stderr $ "Postulate path: " ++ postulateFile

      -- Use bracket to ensure cleanup happens even if test fails
      bracket
        (do
          hPutStrLn stderr "\n--- Agent 1: Creating state ---"
          initServerState)
        cleanupServerState
        (\state1 -> bracket
          (do
            hPutStrLn stderr "\n--- Agent 2: Creating state ---"
            initServerState)
          cleanupServerState
          (\state2 -> do
            -- Agent 1: Load Example.agda and work with it
            hPutStrLn stderr "\n--- Agent 1: Loading Example.agda ---"
            _ <- handleAgdaTool state1 (Types.AgdaLoad
              { Types.file = T.pack examplePath
              , Types.sessionId = Nothing
              , Types.format = Nothing
              })

            -- Agent 2: Load PostulateTest.agda in separate state
            hPutStrLn stderr "\n--- Agent 2: Loading PostulateTest.agda ---"
            _ <- handleAgdaTool state2 (Types.AgdaLoad
              { Types.file = T.pack postulateFile
              , Types.sessionId = Nothing
              , Types.format = Nothing
              })

            -- Agent 1: Get goals from Example.agda
            hPutStrLn stderr "\n--- Agent 1: Getting goals (initial) ---"
            goals1_before <- runTool state1 (Types.AgdaGetGoals
              { Types.sessionId = Nothing
              , Types.format = Nothing
              })

            hPutStrLn stderr $ "Agent 1 goals response: " ++ show goals1_before

            let goalCount1_before = countGoals goals1_before
            hPutStrLn stderr $ "Agent 1 goal count: " ++ show goalCount1_before
            assertBool "Agent 1 should see 5 goals initially" (goalCount1_before == 5)

            -- Agent 1: Fill first goal with 'zero'
            hPutStrLn stderr "\n--- Agent 1: Filling goal 0 with 'zero' ---"
            _ <- handleAgdaTool state1 (Types.AgdaGive
              { Types.goalId = 0
              , Types.expression = "zero"
              , Types.sessionId = Nothing
              , Types.format = Nothing
              })

            -- Agent 2: Get goals from PostulateTest.agda (should be empty)
            hPutStrLn stderr "\n--- Agent 2: Getting goals ---"
            goals2 <- runTool state2 (Types.AgdaGetGoals
              { Types.sessionId = Nothing
              , Types.format = Nothing
              })

            hPutStrLn stderr $ "Agent 2 goals response: " ++ show goals2

            let goalCount2 = countGoals goals2
            hPutStrLn stderr $ "Agent 2 goal count: " ++ show goalCount2
            assertBool "Agent 2 should see 0 goals (all postulates)" (goalCount2 == 0)

            -- Agent 1: Get goals again - should see one less goal (no reload needed)
            hPutStrLn stderr "\n--- Agent 1: Getting goals (after first fill) ---"
            goals1_after <- runTool state1 (Types.AgdaGetGoals
              { Types.sessionId = Nothing
              , Types.format = Nothing
              })

            hPutStrLn stderr $ "Agent 1 goals response (after fill): " ++ show goals1_after

            let goalCount1_after = countGoals goals1_after
            hPutStrLn stderr $ "Agent 1 goal count (after fill): " ++ show goalCount1_after
            assertBool "Agent 1 should see 4 goals after filling one" (goalCount1_after == 4)

            -- Agent 2: List postulates (should work without interference from Agent 1)
            hPutStrLn stderr "\n--- Agent 2: Listing postulates ---"
            postulates <- handleAgdaTool state2 (Types.AgdaListPostulates
              { Types.file = T.pack postulateFile
              , Types.sessionId = Nothing
              , Types.format = Nothing
              })
            case postulates of
              MCP.ContentText txt -> do
                hPutStrLn stderr $ "Agent 2 postulates: " ++ T.unpack (T.take 100 txt)
                assertBool "Agent 2 should see postulates" (T.isInfixOf "postulate" txt || T.isInfixOf "double" txt)
              _ -> assertFailure "Expected ContentText response from list postulates"

            -- Agent 1: Fill another goal (goal 1) with 'suc zero'
            hPutStrLn stderr "\n--- Agent 1: Filling goal 1 with 'suc zero' ---"
            _ <- handleAgdaTool state1 (Types.AgdaGive
              { Types.goalId = 1
              , Types.expression = "suc zero"
              , Types.sessionId = Nothing
              , Types.format = Nothing
              })

            -- Agent 1: Verify state persists (should have 3 goals now)
            hPutStrLn stderr "\n--- Agent 1: Getting goals (after second fill) ---"
            goals1_final <- runTool state1 (Types.AgdaGetGoals
              { Types.sessionId = Nothing
              , Types.format = Nothing
              })

            let goalCount1_final = countGoals goals1_final
            hPutStrLn stderr $ "Agent 1 goal count (final): " ++ show goalCount1_final
            assertBool "Agent 1 should see 3 goals after filling two" (goalCount1_final == 3)

            -- Verify complete isolation: Agent 2 still sees 0 goals
            hPutStrLn stderr "\n--- Agent 2: Getting goals (verification) ---"
            goals2_verify <- runTool state2 (Types.AgdaGetGoals
              { Types.sessionId = Nothing
              , Types.format = Nothing
              })

            let goalCount2_verify = countGoals goals2_verify
            hPutStrLn stderr $ "Agent 2 goal count (verification): " ++ show goalCount2_verify
            assertBool "Agent 2 still has 0 goals (isolation verified)" (goalCount2_verify == 0)

            hPutStrLn stderr "\n=== Multi-Agent Test Completed Successfully ===")  -- Close state2 do block
          )  -- Close state1 bracket
  ]
  where
    countGoals :: JSON.Value -> Int
    countGoals (JSON.Object obj) = do
      let keys = JSON.KeyMap.keys obj
      let _ = unsafePerformIO $ hPutStrLn stderr $ "  Top-level keys in response: " ++ show keys
      -- First look for "info" object, then "visibleGoals" inside it
      case JSON.KeyMap.lookup (JSON.Key.fromString "info") obj of
        Just (JSON.Object infoObj) ->
          let infoKeys = JSON.KeyMap.keys infoObj
              _ = unsafePerformIO $ hPutStrLn stderr $ "  Keys inside 'info': " ++ show infoKeys
          in case JSON.KeyMap.lookup (JSON.Key.fromString "visibleGoals") infoObj of
               Just (JSON.Array arr) -> V.length arr
               _ -> 0
        _ -> 0
    countGoals _ = 0

