{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AgdaMCP.TestUtils
  ( copyTestFile
  , cleanupTestFile
  , withTempTestFile
  ) where

import System.FilePath ((</>), takeDirectory)
import System.Directory (getCurrentDirectory, copyFile, createDirectoryIfMissing, removeDirectoryRecursive, getTemporaryDirectory)
import System.Random (randomIO)
import Control.Exception (SomeException, bracket, catch)

-- ============================================================================
-- Temp File Utilities (for tests that modify files)
-- ============================================================================

-- | Copy test file from test directory to a temporary location
-- Uses random temp directory to preserve module name and avoid conflicts
copyTestFile :: FilePath -> IO FilePath
copyTestFile filename = do
  cwd <- getCurrentDirectory
  tmpDir <- getTemporaryDirectory
  randomHash <- randomIO :: IO Int
  let sourceFile = cwd </> "test" </> filename
  let tempDir = tmpDir </> ("agda-mcp-test-" ++ show (abs randomHash))
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
