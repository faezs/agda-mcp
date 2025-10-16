{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module AgdaMCP.FileEdit
  ( FileEdit(..)
  , applyFileEdits
  , applyReplaceHole
  , applyReplaceLine
  , applyBatchEdits
  , rangeToPositions
  , compareRangeReverse
  , compareFileEditReverse
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import Agda.Syntax.Position (Range, rStart, rEnd, posLine, posCol)

-- | File edit operations with response-specific semantics
data FileEdit
  -- | In-place range replacement (Give, Refine, Intro, Auto, SolveOne)
  -- Removes {! !} braces and replaces with new text
  = ReplaceHole
      { editFile :: FilePath
      , editRange :: Range         -- Agda Range type
      , editReplacement :: Text
      , editKeepBraces :: Bool     -- True for Give_Paren, False for Give_String
      }

  -- | Line replacement with multiple clauses (MakeCase)
  -- Deletes line and inserts N new lines
  | ReplaceLine
      { editFile :: FilePath
      , editLineNumber :: Int      -- 1-indexed line with the goal
      , editClauses :: [Text]      -- New clauses to insert
      , editIndentLevel :: Int     -- Preserve indentation
      , editNeedsReload :: Bool    -- Always True for MakeCase
      }

  -- | Batch of edits applied in reverse position order (SolveAll)
  | BatchEdits
      { editFile :: FilePath
      , editOperations :: [FileEdit]  -- Pre-sorted in reverse order
      }
  deriving (Show)

-- ============================================================================
-- Main Entry Point
-- ============================================================================

-- | Apply file edits with appropriate strategy for each type
applyFileEdits :: [FileEdit] -> IO ()
applyFileEdits edits = mapM_ applyFileEdit edits
  where
    applyFileEdit :: FileEdit -> IO ()
    applyFileEdit edit = case edit of
      ReplaceHole{..} -> applyReplaceHole editFile editRange editReplacement editKeepBraces
      ReplaceLine{..} -> applyReplaceLine editFile editLineNumber editClauses editIndentLevel editNeedsReload
      BatchEdits{..}  -> applyBatchEdits editFile editOperations

-- ============================================================================
-- Strategy 1: ReplaceHole (in-place replacement, no structural change)
-- ============================================================================

-- | Replace hole content at range
-- If keepBraces=True: wrap in parens, keep {! !}
-- If keepBraces=False: remove {! !}, replace with text
applyReplaceHole :: FilePath -> Range -> Text -> Bool -> IO ()
applyReplaceHole file range replacement keepBraces = do
  content <- TIO.readFile file
  let lines' = T.lines content
  let (sl, sc, el, ec) = rangeToPositions range

  if keepBraces
    then do
      -- Give_Paren: Keep {! !}, wrap content in parens
      -- Input:  {! expr !}
      -- Output: {! (expr) !}
      let newLines = replaceWithinBraces lines' sl sc el ec replacement
      TIO.writeFile file (T.unlines newLines)
    else do
      -- Give_String: Remove {! !} entirely, replace with text
      -- Input:  {! old !}
      -- Output: new
      let newLines = replaceBracesAndContent lines' sl sc el ec replacement
      TIO.writeFile file (T.unlines newLines)

-- | Replace content within {! !}, keep braces
replaceWithinBraces :: [Text] -> Int -> Int -> Int -> Int -> Text -> [Text]
replaceWithinBraces lines' sl sc el ec newText
  | sl == el =
      -- Single line: {! old !} → {! (old) !}
      let line = lines' !! (sl - 1)
          before = T.take (sc - 1 + 2) line  -- Include "{!"
          after = T.drop (ec - 1 - 2) line    -- Include "!}"
          middle = T.drop (sc - 1 + 2) (T.take (ec - 1 - 2) line)
          wrapped = if T.null middle then newText else "(" <> middle <> ")"
          newLine = before <> wrapped <> after
      in take (sl - 1) lines' ++ [newLine] ++ drop sl lines'
  | otherwise =
      -- Multi-line holes are rare, handle as single-line for now
      replaceWithinBraces lines' sl sc sl ec newText

-- | Replace {! !} and content with new text
replaceBracesAndContent :: [Text] -> Int -> Int -> Int -> Int -> Text -> [Text]
replaceBracesAndContent lines' sl sc el ec newText
  | sl == el =
      -- Single line: {! old !} → new
      let line = lines' !! (sl - 1)
          before = T.take (sc - 1) line
          after = T.drop (ec - 1) line
          newLine = before <> newText <> after
      in take (sl - 1) lines' ++ [newLine] ++ drop sl lines'
  | otherwise =
      -- Multi-line: take before first line, after last line
      let firstLine = lines' !! (sl - 1)
          lastLine = lines' !! (el - 1)
          before = T.take (sc - 1) firstLine
          after = T.drop (ec - 1) lastLine
          newLine = before <> newText <> after
      in take (sl - 1) lines' ++ [newLine] ++ drop el lines'

-- ============================================================================
-- Strategy 2: ReplaceLine (structural change, line deletion + insertion)
-- ============================================================================

-- | Replace line with multiple clauses (case split)
-- Deletes the line at lineNumber, inserts N new clauses with indentation
-- Note: indentLevel parameter is ignored - we calculate from the original line
applyReplaceLine :: FilePath -> Int -> [Text] -> Int -> Bool -> IO ()
applyReplaceLine file lineNum clauses _indentLevel _needsReload = do
  content <- TIO.readFile file
  let lines' = T.lines content

  if lineNum < 1 || lineNum > length lines'
    then putStrLn $ "Warning: Line number " ++ show lineNum ++ " out of bounds"
    else do
      -- Split at line position
      let (beforeLines, originalLine:afterLines) = splitAt (lineNum - 1) lines'

      -- Calculate indentation from the original line (leading whitespace)
      let originalIndent = T.takeWhile (== ' ') originalLine
      let indentLevel = T.length originalIndent

      -- Apply the same indentation to each clause
      -- Note: Agda provides clauses without leading whitespace
      let indentedClauses = map (originalIndent <>) clauses

      -- Reconstruct file
      let newContent = T.unlines $ beforeLines ++ indentedClauses ++ afterLines
      TIO.writeFile file newContent

      putStrLn $ "Replaced line " ++ show lineNum ++ " with " ++
                 show (length clauses) ++ " clauses (indent level: " ++
                 show indentLevel ++ ")"

-- ============================================================================
-- Strategy 3: BatchEdits (reverse-order application)
-- ============================================================================

-- | Apply batch of edits in reverse position order
-- Ensures earlier edits don't invalidate later positions
applyBatchEdits :: FilePath -> [FileEdit] -> IO ()
applyBatchEdits file ops = do
  putStrLn $ "Applying " ++ show (length ops) ++ " batch edits in reverse order"
  -- Operations should already be sorted in reverse order
  -- Apply each sequentially
  forM_ ops $ \op -> case op of
    ReplaceHole{..} -> applyReplaceHole file editRange editReplacement editKeepBraces
    _ -> putStrLn "Warning: BatchEdits should only contain ReplaceHole operations"
  where
    forM_ = flip mapM_

-- ============================================================================
-- Utility Functions
-- ============================================================================

-- | Convert Agda Range to 1-indexed positions (startLine, startCol, endLine, endCol)
rangeToPositions :: Range -> (Int, Int, Int, Int)
rangeToPositions range =
  case (rStart range, rEnd range) of
    (Just startPos, Just endPos) ->
      (fromIntegral (posLine startPos),
       fromIntegral (posCol startPos),
       fromIntegral (posLine endPos),
       fromIntegral (posCol endPos))
    _ -> (0, 0, 0, 0)

-- | Compare ranges in reverse order (for bottom-to-top application)
-- Later positions (higher line/col) should be processed first
compareRangeReverse :: Range -> Range -> Ordering
compareRangeReverse r1 r2 =
  let (l1, c1, _, _) = rangeToPositions r1
      (l2, c2, _, _) = rangeToPositions r2
  in compare (Down l1, Down c1) (Down l2, Down c2)

-- | Compare FileEdit by range position (for sorting)
compareFileEditReverse :: FileEdit -> FileEdit -> Ordering
compareFileEditReverse (ReplaceHole _ r1 _ _) (ReplaceHole _ r2 _ _) =
  compareRangeReverse r1 r2
compareFileEditReverse _ _ = EQ  -- Non-ReplaceHole edits don't need sorting
