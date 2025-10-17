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
import System.FilePath ((</>), takeDirectory, takeFileName)
import System.Directory (getCurrentDirectory, copyFile, createDirectoryIfMissing, removeFile, removeDirectoryRecursive, getTemporaryDirectory)
import System.Random (randomIO)
import Control.Exception (try, SomeException, bracket, catch)

import AgdaMCP.Server
import qualified AgdaMCP.Types as Types
import qualified AgdaMCP.SessionManager as SessionManager
import qualified MCP.Server as MCP
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

-- | Custom version for edit-persistence subdirectory
copyEditTestFile :: FilePath -> IO FilePath
copyEditTestFile filename = do
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
cleanupEditTestFile :: FilePath -> IO ()
cleanupEditTestFile filepath = do
  let tempDir = takeDirectory filepath
  Control.Exception.catch (removeDirectoryRecursive tempDir) (\(_ :: SomeException) -> pure ())

-- | Bracket for edit-persistence temp file operations
withTempEditTestFile :: FilePath -> (FilePath -> IO a) -> IO a
withTempEditTestFile filename = bracket (copyEditTestFile filename) cleanupEditTestFile

-- | Load a file
loadFile :: SessionManager.SessionManager ServerState -> Text -> FilePath -> IO ()
loadFile manager sessionId filepath = do
  let tool = Types.AgdaLoad { Types.file = T.pack filepath, Types.sessionId = Just sessionId, Types.format = Just "Full" }
  _ <- handleAgdaToolWithSession manager tool
  pure ()

-- | Get goal count from a load response
getGoalCount :: SessionManager.SessionManager ServerState -> Text -> IO Int
getGoalCount manager sessionId = do
  let tool = Types.AgdaGetGoals { Types.sessionId = Just sessionId, Types.format = Just "Full" }
  result <- handleAgdaToolWithSession manager tool
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
executeGive :: SessionManager.SessionManager ServerState -> Text -> Int -> Text -> IO ()
executeGive manager sessionId goalId expr = do
  let tool = Types.AgdaGive { Types.goalId = goalId, Types.expression = expr, Types.sessionId = Just sessionId, Types.format = Nothing }
  _ <- handleAgdaToolWithSession manager tool
  pure ()

-- | Execute refine command
executeRefine :: SessionManager.SessionManager ServerState -> Text -> Int -> Text -> IO ()
executeRefine manager sessionId goalId expr = do
  let tool = Types.AgdaRefine { Types.goalId = goalId, Types.expression = expr, Types.sessionId = Just sessionId, Types.format = Nothing }
  _ <- handleAgdaToolWithSession manager tool
  pure ()

-- | Execute case split command
executeCaseSplit :: SessionManager.SessionManager ServerState -> Text -> Int -> Text -> IO ()
executeCaseSplit manager sessionId goalId var = do
  let tool = Types.AgdaCaseSplit { Types.goalId = goalId, Types.variable = var, Types.sessionId = Just sessionId, Types.format = Nothing }
  _ <- handleAgdaToolWithSession manager tool
  pure ()

-- ============================================================================
-- Test Suite
-- ============================================================================

-- | SessionManager fixture for tests
withSessionManager :: (IO (SessionManager.SessionManager ServerState) -> TestTree) -> TestTree
withSessionManager = withResource initSessionManager SessionManager.destroySessionManager

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
replaceHoleTests = withSessionManager $ \getManager -> testGroup "ReplaceHole Edit Tests"
  [ simpleTestCase "simple give fills hole and file reloads" $ do
      manager <- getManager
      withTempEditTestFile "EditPersistenceTest.agda" $ \testFile -> do
        let sessionId = "test-edit-simple-give"

        -- Load file
        loadFile manager sessionId testFile
        goalsBefore <- getGoalCount manager sessionId
        assertEqual "Should have 10 goals initially" 10 goalsBefore

        -- Give: Fill goal 0 (simpleGive) with "true"
        executeGive manager sessionId 0 "true"

        -- Verify file edit
        content <- TIO.readFile testFile
        assertContains "simpleGive = true" content

        -- Reload and verify
        loadFile manager sessionId testFile
        goalsAfter <- getGoalCount manager sessionId
        assertEqual "Should have 9 goals after give" 9 goalsAfter

  , simpleTestCase "give with expression persists correctly" $ do
      manager <- getManager
      withTempEditTestFile "EditPersistenceTest.agda" $ \testFile -> do
        let sessionId = "test-edit-give-expression"
        loadFile manager sessionId testFile

        -- Give: Fill goal 1 (refineTest) with "suc zero"
        executeGive manager sessionId 1 "suc zero"

        -- Verify file contains exact expression
        content <- TIO.readFile testFile
        assertContains "refineTest = suc zero" content

        -- Reload succeeds
        loadFile manager sessionId testFile

  , simpleTestCase "refine persists and creates new holes" $ do
      manager <- getManager
      withTempEditTestFile "EditPersistenceTest.agda" $ \testFile -> do
        let sessionId = "test-edit-refine"
        loadFile manager sessionId testFile

        -- Refine: Apply "suc" to goal 1 (refineTest)
        executeRefine manager sessionId 1 "suc"

        -- Verify file edit: Should have "suc ?" (Agda uses ? not {! !})
        content <- TIO.readFile testFile
        assertContains "refineTest = suc ?" content

        -- Reload and verify goal structure
        loadFile manager sessionId testFile
        goals <- getGoalCount manager sessionId
        -- Should still have same number (refined one, created new sub-goal)
        assertEqual "Goal count should be same (refined but added subgoal)" 10 goals

  , simpleTestCase "give with unicode persists correctly" $ do
      manager <- getManager
      withTempEditTestFile "EditPersistenceTest.agda" $ \testFile -> do
        let sessionId = "test-edit-unicode"
        loadFile manager sessionId testFile

        -- Give: Fill goal 7 (unicodeTest) with "tt"
        executeGive manager sessionId 7 "tt"

        -- Verify UTF-8 encoding preserved
        content <- TIO.readFile testFile
        assertContains "unicodeTest = tt" content
        assertContains "data ⊤" content  -- Original Unicode preserved

        -- Reload succeeds
        loadFile manager sessionId testFile

  , simpleTestCase "give with complex expression" $ do
      manager <- getManager
      withTempEditTestFile "EditPersistenceTest.agda" $ \testFile -> do
        let sessionId = "test-edit-complex-expression"
        loadFile manager sessionId testFile

        -- Give: Fill expressionTest with nested suc
        let longExpr = "suc (suc (suc (suc zero)))"
        executeGive manager sessionId 9 longExpr

        -- Verify persists correctly
        content <- TIO.readFile testFile
        assertContains longExpr content

        -- Reload succeeds
        loadFile manager sessionId testFile
  ]

-- ============================================================================
-- ReplaceLine Tests (Case Split)
-- ============================================================================

replaceLineTests :: TestTree
replaceLineTests = withSessionManager $ \getManager -> testGroup "ReplaceLine Edit Tests"
  [ simpleTestCase "case split on Nat creates two clauses" $ do
      manager <- getManager
      withTempEditTestFile "EditPersistenceTest.agda" $ \testFile -> do
        let sessionId = "test-edit-case-split"
        loadFile manager sessionId testFile

        -- Case split on "_+_" function, goal 2, variable "n"
        executeCaseSplit manager sessionId 2 "n"

        -- Verify file edit: Line replaced with 2 clauses
        -- Note: Agda generates ? instead of {! !} in case split
        content <- TIO.readFile testFile
        assertContains "zero + m = ?" content
        assertContains "suc n + m = ?" content
        assertNotContains "n + m = {! !}" content  -- Original line gone

        -- Reload and verify goal structure changed
        loadFile manager sessionId testFile
        goalsAfter <- getGoalCount manager sessionId
        -- Should have 11 goals (was 10, removed 1, added 2)
        assertEqual "Should have 11 goals after split" 11 goalsAfter

  , simpleTestCase "case split preserves indentation" $ do
      manager <- getManager
      withTempEditTestFile "EditPersistenceTest.agda" $ \testFile -> do
        let sessionId = "test-edit-case-split-indentation"
        loadFile manager sessionId testFile

        -- Case split on multiply (goal 3)
        executeCaseSplit manager sessionId 3 "n"

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
        loadFile manager sessionId testFile

  , simpleTestCase "nested case split" $ do
      manager <- getManager
      withTempEditTestFile "EditPersistenceTest.agda" $ \testFile -> do
        let sessionId = "test-edit-nested-case-split"
        loadFile manager sessionId testFile

        -- First split on "n" in nestedSplit (goal 8)
        executeCaseSplit manager sessionId 8 "n"

        -- Reload to get new goal structure
        loadFile manager sessionId testFile
        goalsAfter1 <- getGoalCount manager sessionId
        assertEqual "Should have 11 goals after first split" 11 goalsAfter1

        -- After split: nestedSplit n m removed, replaced by 2 clauses
        -- New structure: 0-7 unchanged, 8: nestedSplit zero m, 9: nestedSplit (suc n) m, 10: expressionTest
        -- Now split on "m" in the zero clause (goal 8)
        executeCaseSplit manager sessionId 8 "m"

        -- Verify file has 3 clauses total (2 from first, 1 from second split becomes 2)
        -- Note: Agda generates ? instead of {! !} in case split
        content <- TIO.readFile testFile
        assertContains "nestedSplit zero zero = ?" content
        assertContains "nestedSplit zero (suc m) = ?" content
        assertContains "nestedSplit (suc n) m = ?" content

        -- Reload succeeds
        loadFile manager sessionId testFile
  ]

-- ============================================================================
-- Reload Verification Tests
-- ============================================================================

reloadVerificationTests :: TestTree
reloadVerificationTests = withSessionManager $ \getManager -> testGroup "Reload Verification Tests"
  [ simpleTestCase "sequential gives with reloads between" $ do
      manager <- getManager
      withTempEditTestFile "EditPersistenceTest.agda" $ \testFile -> do
        let sessionId = "test-edit-sequential-gives"

        -- Load -> Give -> Reload -> Give -> Reload
        loadFile manager sessionId testFile
        goals1 <- getGoalCount manager sessionId
        assertEqual "Initial: 10 goals" 10 goals1

        executeGive manager sessionId 0 "true"
        loadFile manager sessionId testFile
        goals2 <- getGoalCount manager sessionId
        assertEqual "After first give: 9 goals" 9 goals2

        executeGive manager sessionId 0 "zero"  -- New goal 0 after first give removed goal 0
        loadFile manager sessionId testFile
        goals3 <- getGoalCount manager sessionId
        assertEqual "After second give: 8 goals" 8 goals3

        -- Verify file has both edits
        content <- TIO.readFile testFile
        assertContains "simpleGive = true" content
        assertContains "refineTest = zero" content

  , simpleTestCase "case split then reload then fill new goal" $ do
      manager <- getManager
      withTempEditTestFile "EditPersistenceTest.agda" $ \testFile -> do
        let sessionId = "test-edit-split-reload-fill"
        loadFile manager sessionId testFile

        -- Case split on addition (goal 2: n + m)
        executeCaseSplit manager sessionId 2 "n"

        -- MUST reload to see new goal structure
        loadFile manager sessionId testFile
        goalsAfter <- getGoalCount manager sessionId
        assertEqual "After split: 11 goals" 11 goalsAfter

        -- After split: goal 2 removed, replaced by 2 new goals at positions 2 and 3
        -- New structure: 0:simpleGive, 1:refineTest, 2:zero+m, 3:suc n+m, 4:n*m, ...
        -- Now fill the zero clause (goal 2)
        executeGive manager sessionId 2 "m"

        -- Verify file has split + fill
        -- Note: After give, the filled clause has value; unfilled has ?
        content <- TIO.readFile testFile
        assertContains "zero + m = m" content
        assertContains "suc n + m = ?" content

        -- Final reload succeeds
        loadFile manager sessionId testFile
        goalsFinal <- getGoalCount manager sessionId
        assertEqual "After filling one clause: 10 goals" 10 goalsFinal
  ]

-- ============================================================================
-- Edge Case Tests
-- ============================================================================

edgeCaseTests :: TestTree
edgeCaseTests = withSessionManager $ \getManager -> testGroup "Edge Case Tests"
  [ simpleTestCase "give then refine same definition" $ do
      manager <- getManager
      withTempEditTestFile "EditPersistenceTest.agda" $ \testFile -> do
        let sessionId = "test-edit-give-then-refine"
        loadFile manager sessionId testFile

        -- Give to refineTest
        executeGive manager sessionId 1 "zero"

        -- Reload
        loadFile manager sessionId testFile
        content1 <- TIO.readFile testFile
        assertContains "refineTest = zero" content1

        -- Note: Can't refine the same hole after giving, so this test
        -- just verifies the give persisted correctly

  , simpleTestCase "case split with same variable name" $ do
      manager <- getManager
      withTempEditTestFile "EditPersistenceTest.agda" $ \testFile -> do
        let sessionId = "test-edit-split-same-variable"
        loadFile manager sessionId testFile

        -- Split both + and * on "n" with reload between
        executeCaseSplit manager sessionId 2 "n"
        loadFile manager sessionId testFile  -- Reload to update goal structure

        -- After first split: goal 2 (n+m) removed, replaced by 2 new goals
        -- New structure: 0:simpleGive, 1:refineTest, 2:zero+m, 3:suc n+m, 4:n*m, ...
        -- So n*m is now goal 4
        executeCaseSplit manager sessionId 4 "n"
        loadFile manager sessionId testFile

        -- Both splits should be in file
        -- Note: Agda generates ? instead of {! !} in case split
        content <- TIO.readFile testFile
        assertContains "zero + m = ?" content
        assertContains "suc n + m = ?" content
        assertContains "zero * m = ?" content
        assertContains "suc n * m = ?" content
  ]
