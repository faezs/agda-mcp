{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AgdaMCP.EditPersistenceSpec (tests) where

import Test.Tasty
import Test.Tasty.Providers
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Key as JSON.Key
import qualified Data.Aeson.KeyMap as JSON.KeyMap
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Data.IORef
import System.FilePath ((</>), takeDirectory, takeFileName)
import System.Directory (getCurrentDirectory, copyFile, createDirectoryIfMissing, removeFile, removeDirectoryRecursive, getTemporaryDirectory)
import System.Random (randomIO)
import Control.Exception (try, SomeException, bracket, catch)

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

assertContains :: Text -> Text -> IO ()
assertContains needle haystack =
  if needle `T.isInfixOf` haystack
    then pure ()
    else error $ "Expected to find '" ++ T.unpack needle ++ "' in content.\nActual content:\n" ++ T.unpack haystack

assertNotContains :: Text -> Text -> IO ()
assertNotContains needle haystack =
  if not (needle `T.isInfixOf` haystack)
    then pure ()
    else error $ "Expected NOT to find '" ++ T.unpack needle ++ "' in content"

-- ============================================================================
-- Test Utilities
-- ============================================================================

-- | Copy test file from edit-persistence directory to a temporary location
-- Uses random temp directory to preserve module name and avoid conflicts
copyTestFile :: FilePath -> IO FilePath
copyTestFile filename = do
  cwd <- getCurrentDirectory
  tmpDir <- getTemporaryDirectory
  randomHash <- randomIO :: IO Int
  let sourceFile = cwd </> "test" </> "edit-persistence" </> filename
  let tempDir = tmpDir </> ("agda-mcp-persist-test-" ++ show (abs randomHash))
  let tempFile = tempDir </> filename
  createDirectoryIfMissing True tempDir
  copyFile sourceFile tempFile
  pure tempFile

-- | Clean up temp directory
cleanupTestFile :: FilePath -> IO ()
cleanupTestFile filepath = do
  let tempDir = takeDirectory filepath
  Control.Exception.catch (removeDirectoryRecursive tempDir) (\(_ :: SomeException) -> pure ())

-- | Bracket for temp file operations
withTempTestFile :: FilePath -> (FilePath -> IO a) -> IO a
withTempTestFile filename = bracket (copyTestFile filename) cleanupTestFile

-- | Load a file
loadFile :: IORef ServerState -> FilePath -> IO ()
loadFile stateRef filepath = do
  let tool = Types.AgdaLoad { Types.file = T.pack filepath, Types.format = Just "Full" }
  _ <- handleAgdaTool stateRef tool
  pure ()

-- | Get goal count from a load response
getGoalCount :: IORef ServerState -> IO Int
getGoalCount stateRef = do
  let tool = Types.AgdaGetGoals { Types.format = Just "Full" }
  result <- handleAgdaTool stateRef tool
  case result of
    MCP.ContentText txt -> do
      case JSON.decode (LBS.fromStrict $ TE.encodeUtf8 txt) of
        Just (JSON.Object obj) -> do
          case JSON.KeyMap.lookup (JSON.Key.fromText "info") obj of
            Just (JSON.Object info) -> do
              case JSON.KeyMap.lookup (JSON.Key.fromText "visibleGoals") info of
                Just (JSON.Array goals) -> pure (length goals)
                _ -> pure 0
            _ -> pure 0
        _ -> pure 0
    _ -> pure 0

-- | Execute give command
executeGive :: IORef ServerState -> Int -> Text -> IO ()
executeGive stateRef goalId expr = do
  let tool = Types.AgdaGive { Types.goalId = goalId, Types.expression = expr, Types.format = Nothing }
  _ <- handleAgdaTool stateRef tool
  pure ()

-- | Execute refine command
executeRefine :: IORef ServerState -> Int -> Text -> IO ()
executeRefine stateRef goalId expr = do
  let tool = Types.AgdaRefine { Types.goalId = goalId, Types.expression = expr, Types.format = Nothing }
  _ <- handleAgdaTool stateRef tool
  pure ()

-- | Execute case split command
executeCaseSplit :: IORef ServerState -> Int -> Text -> IO ()
executeCaseSplit stateRef goalId var = do
  let tool = Types.AgdaCaseSplit { Types.goalId = goalId, Types.variable = var, Types.format = Nothing }
  _ <- handleAgdaTool stateRef tool
  pure ()

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Edit Persistence Tests"
  [ replaceHoleTests
  , replaceLineTests
  , reloadVerificationTests
  , edgeCaseTests
  ]

-- ============================================================================
-- ReplaceHole Tests (Give, Refine, Auto)
-- ============================================================================

replaceHoleTests :: TestTree
replaceHoleTests = testGroup "ReplaceHole Edit Tests"
  [ simpleTestCase "simple give fills hole and file reloads" $ do
      withTempTestFile "EditPersistenceTest.agda" $ \testFile -> do
        stateRef <- initServerState

        -- Load file
        loadFile stateRef testFile
        goalsBefore <- getGoalCount stateRef
        assertEqual "Should have 10 goals initially" 10 goalsBefore

        -- Give: Fill goal 0 (simpleGive) with "true"
        executeGive stateRef 0 "true"

        -- Verify file edit
        content <- TIO.readFile testFile
        assertContains "simpleGive = true" content

        -- Reload and verify
        loadFile stateRef testFile
        goalsAfter <- getGoalCount stateRef
        assertEqual "Should have 9 goals after give" 9 goalsAfter

  , simpleTestCase "give with expression persists correctly" $ do
      withTempTestFile "EditPersistenceTest.agda" $ \testFile -> do
        stateRef <- initServerState
        loadFile stateRef testFile

        -- Give: Fill goal 1 (refineTest) with "suc zero"
        executeGive stateRef 1 "suc zero"

        -- Verify file contains exact expression
        content <- TIO.readFile testFile
        assertContains "refineTest = suc zero" content

        -- Reload succeeds
        loadFile stateRef testFile

  , simpleTestCase "refine persists and creates new holes" $ do
      withTempTestFile "EditPersistenceTest.agda" $ \testFile -> do
        stateRef <- initServerState
        loadFile stateRef testFile

        -- Refine: Apply "suc" to goal 1 (refineTest)
        executeRefine stateRef 1 "suc"

        -- Verify file edit: Should have "suc ?" (Agda uses ? not {! !})
        content <- TIO.readFile testFile
        assertContains "refineTest = suc ?" content

        -- Reload and verify goal structure
        loadFile stateRef testFile
        goals <- getGoalCount stateRef
        -- Should still have same number (refined one, created new sub-goal)
        assertEqual "Goal count should be same (refined but added subgoal)" 10 goals

  , simpleTestCase "give with unicode persists correctly" $ do
      withTempTestFile "EditPersistenceTest.agda" $ \testFile -> do
        stateRef <- initServerState
        loadFile stateRef testFile

        -- Give: Fill goal 7 (unicodeTest) with "tt"
        executeGive stateRef 7 "tt"

        -- Verify UTF-8 encoding preserved
        content <- TIO.readFile testFile
        assertContains "unicodeTest = tt" content
        assertContains "data ⊤" content  -- Original Unicode preserved

        -- Reload succeeds
        loadFile stateRef testFile

  , simpleTestCase "give with complex expression" $ do
      withTempTestFile "EditPersistenceTest.agda" $ \testFile -> do
        stateRef <- initServerState
        loadFile stateRef testFile

        -- Give: Fill expressionTest with nested suc
        let longExpr = "suc (suc (suc (suc zero)))"
        executeGive stateRef 9 longExpr

        -- Verify persists correctly
        content <- TIO.readFile testFile
        assertContains longExpr content

        -- Reload succeeds
        loadFile stateRef testFile
  ]

-- ============================================================================
-- ReplaceLine Tests (Case Split)
-- ============================================================================

replaceLineTests :: TestTree
replaceLineTests = testGroup "ReplaceLine Edit Tests"
  [ simpleTestCase "case split on Nat creates two clauses" $ do
      withTempTestFile "EditPersistenceTest.agda" $ \testFile -> do
        stateRef <- initServerState
        loadFile stateRef testFile

        -- Case split on "_+_" function, goal 2, variable "n"
        executeCaseSplit stateRef 2 "n"

        -- Verify file edit: Line replaced with 2 clauses
        -- Note: Agda generates ? instead of {! !} in case split
        content <- TIO.readFile testFile
        assertContains "zero + m = ?" content
        assertContains "suc n + m = ?" content
        assertNotContains "n + m = {! !}" content  -- Original line gone

        -- Reload and verify goal structure changed
        loadFile stateRef testFile
        goalsAfter <- getGoalCount stateRef
        -- Should have 11 goals (was 10, removed 1, added 2)
        assertEqual "Should have 11 goals after split" 11 goalsAfter

  , simpleTestCase "case split preserves indentation" $ do
      withTempTestFile "EditPersistenceTest.agda" $ \testFile -> do
        stateRef <- initServerState
        loadFile stateRef testFile

        -- Case split on multiply (goal 3)
        executeCaseSplit stateRef 3 "n"

        -- Verify indentation matches
        content <- TIO.readFile testFile
        let contentLines = T.lines content
        let zeroLine = filter (T.isInfixOf "zero * m") contentLines
        let sucLine = filter (T.isInfixOf "suc n * m") contentLines

        assertBool "Should have zero clause" (not $ null zeroLine)
        assertBool "Should have suc clause" (not $ null sucLine)

        -- Both should be at top level (column 0)
        assertBool "zero clause starts at column 0" $ case zeroLine of
          (line:_) -> T.take 1 line /= " "
          _ -> False

        -- Reload succeeds
        loadFile stateRef testFile

  , simpleTestCase "nested case split" $ do
      withTempTestFile "EditPersistenceTest.agda" $ \testFile -> do
        stateRef <- initServerState
        loadFile stateRef testFile

        -- First split on "n" in nestedSplit (goal 8)
        executeCaseSplit stateRef 8 "n"

        -- Reload to get new goal structure
        loadFile stateRef testFile
        goalsAfter1 <- getGoalCount stateRef
        assertEqual "Should have 11 goals after first split" 11 goalsAfter1

        -- After split: nestedSplit n m removed, replaced by 2 clauses
        -- New structure: 0-7 unchanged, 8: nestedSplit zero m, 9: nestedSplit (suc n) m, 10: expressionTest
        -- Now split on "m" in the zero clause (goal 8)
        executeCaseSplit stateRef 8 "m"

        -- Verify file has 3 clauses total (2 from first, 1 from second split becomes 2)
        -- Note: Agda generates ? instead of {! !} in case split
        content <- TIO.readFile testFile
        assertContains "nestedSplit zero zero = ?" content
        assertContains "nestedSplit zero (suc m) = ?" content
        assertContains "nestedSplit (suc n) m = ?" content

        -- Reload succeeds
        loadFile stateRef testFile
  ]

-- ============================================================================
-- Reload Verification Tests
-- ============================================================================

reloadVerificationTests :: TestTree
reloadVerificationTests = testGroup "Reload Verification Tests"
  [ simpleTestCase "sequential gives with reloads between" $ do
      withTempTestFile "EditPersistenceTest.agda" $ \testFile -> do
        stateRef <- initServerState

        -- Load -> Give -> Reload -> Give -> Reload
        loadFile stateRef testFile
        goals1 <- getGoalCount stateRef
        assertEqual "Initial: 10 goals" 10 goals1

        executeGive stateRef 0 "true"
        loadFile stateRef testFile
        goals2 <- getGoalCount stateRef
        assertEqual "After first give: 9 goals" 9 goals2

        executeGive stateRef 0 "zero"  -- New goal 0 after first give removed goal 0
        loadFile stateRef testFile
        goals3 <- getGoalCount stateRef
        assertEqual "After second give: 8 goals" 8 goals3

        -- Verify file has both edits
        content <- TIO.readFile testFile
        assertContains "simpleGive = true" content
        assertContains "refineTest = zero" content

  , simpleTestCase "case split then reload then fill new goal" $ do
      withTempTestFile "EditPersistenceTest.agda" $ \testFile -> do
        stateRef <- initServerState
        loadFile stateRef testFile

        -- Case split on addition (goal 2: n + m)
        executeCaseSplit stateRef 2 "n"

        -- MUST reload to see new goal structure
        loadFile stateRef testFile
        goalsAfter <- getGoalCount stateRef
        assertEqual "After split: 11 goals" 11 goalsAfter

        -- After split: goal 2 removed, replaced by 2 new goals at positions 2 and 3
        -- New structure: 0:simpleGive, 1:refineTest, 2:zero+m, 3:suc n+m, 4:n*m, ...
        -- Now fill the zero clause (goal 2)
        executeGive stateRef 2 "m"

        -- Verify file has split + fill
        -- Note: After give, the filled clause has value; unfilled has ?
        content <- TIO.readFile testFile
        assertContains "zero + m = m" content
        assertContains "suc n + m = ?" content

        -- Final reload succeeds
        loadFile stateRef testFile
        goalsFinal <- getGoalCount stateRef
        assertEqual "After filling one clause: 10 goals" 10 goalsFinal
  ]

-- ============================================================================
-- Edge Case Tests
-- ============================================================================

edgeCaseTests :: TestTree
edgeCaseTests = testGroup "Edge Case Tests"
  [ simpleTestCase "give then refine same definition" $ do
      withTempTestFile "EditPersistenceTest.agda" $ \testFile -> do
        stateRef <- initServerState
        loadFile stateRef testFile

        -- Give to refineTest
        executeGive stateRef 1 "zero"

        -- Reload
        loadFile stateRef testFile
        content1 <- TIO.readFile testFile
        assertContains "refineTest = zero" content1

        -- Note: Can't refine the same hole after giving, so this test
        -- just verifies the give persisted correctly

  , simpleTestCase "case split with same variable name" $ do
      withTempTestFile "EditPersistenceTest.agda" $ \testFile -> do
        stateRef <- initServerState
        loadFile stateRef testFile

        -- Split both + and * on "n" with reload between
        executeCaseSplit stateRef 2 "n"
        loadFile stateRef testFile  -- Reload to update goal structure

        -- After first split: goal 2 (n+m) removed, replaced by 2 new goals
        -- New structure: 0:simpleGive, 1:refineTest, 2:zero+m, 3:suc n+m, 4:n*m, ...
        -- So n*m is now goal 4
        executeCaseSplit stateRef 4 "n"
        loadFile stateRef testFile

        -- Both splits should be in file
        -- Note: Agda generates ? instead of {! !} in case split
        content <- TIO.readFile testFile
        assertContains "zero + m = ?" content
        assertContains "suc n + m = ?" content
        assertContains "zero * m = ?" content
        assertContains "suc n * m = ?" content
  ]
