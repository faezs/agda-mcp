{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- | Source Document Operations for the Autoformalizer Protocol
--
-- This module implements read-only operations on source documents being formalized.
-- Source documents can be plain text, LaTeX, or Markdown files containing
-- mathematical content to be formalized in Agda.
--
-- Operations:
--   * peek_section: Get content of a named section
--   * grep_source: Search for pattern matches in source
--   * get_theorem: Extract a theorem statement and optional proof
--   * get_dependencies: Find theorems that a given theorem depends on

module AgdaMCP.Autoformalizer.Source
    ( -- * Source State
      SourceDocument(..)
    , Section(..)
    , Theorem(..)
    , emptySourceDocument
    , loadSourceDocument
    , loadSourceFromFile

      -- * Source Operations
    , peekSection
    , grepSource
    , getTheorem
    , getDependencies

      -- * Parsing Utilities
    , parseMarkdownSections
    , parseLatexSections
    , extractMathExpressions
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.List (sortOn)
import Control.Exception (try, SomeException)

import AgdaMCP.Autoformalizer.Types

--------------------------------------------------------------------------------
-- Source Document State
--------------------------------------------------------------------------------

-- | A section within a source document
data Section = Section
    { sectionId :: SectionId
    , sectionTitle :: Text
    , sectionContent :: Text
    , sectionStartPos :: Int
    , sectionEndPos :: Int
    , sectionChildren :: [SectionId]
    }
    deriving (Show, Eq)

-- | A theorem or definition extracted from the source
data Theorem = Theorem
    { thmId :: TheoremId
    , thmSection :: SectionId
    , thmStatement :: Text
    , thmProof :: Maybe Text
    , thmDependencies :: [TheoremId]
    , thmStartPos :: Int
    , thmEndPos :: Int
    }
    deriving (Show, Eq)

-- | Representation of a source document being formalized
data SourceDocument = SourceDocument
    { sourceFilePath :: Maybe FilePath
    , sourceRawContent :: Text
    , sourceSections :: Map SectionId Section
    , sourceTheorems :: Map TheoremId Theorem
    , sourceSectionOrder :: [SectionId]  -- ^ For iteration order
    }
    deriving (Show, Eq)

-- | Empty source document
emptySourceDocument :: SourceDocument
emptySourceDocument = SourceDocument
    { sourceFilePath = Nothing
    , sourceRawContent = ""
    , sourceSections = Map.empty
    , sourceTheorems = Map.empty
    , sourceSectionOrder = []
    }

--------------------------------------------------------------------------------
-- Loading Source Documents
--------------------------------------------------------------------------------

-- | Load a source document from text content
loadSourceDocument :: Text -> SourceDocument
loadSourceDocument content =
    let sections = detectAndParseSections content
        theorems = extractTheorems content sections
    in SourceDocument
        { sourceFilePath = Nothing
        , sourceRawContent = content
        , sourceSections = Map.fromList [(sectionId s, s) | s <- sections]
        , sourceTheorems = Map.fromList [(thmId t, t) | t <- theorems]
        , sourceSectionOrder = map sectionId sections
        }

-- | Load a source document from a file
loadSourceFromFile :: FilePath -> IO (Either Text SourceDocument)
loadSourceFromFile path = do
    result <- try (TIO.readFile path) :: IO (Either SomeException Text)
    case result of
        Left err -> return $ Left $ "Failed to read file: " <> T.pack (show err)
        Right content ->
            let doc = loadSourceDocument content
            in return $ Right doc { sourceFilePath = Just path }

-- | Detect document format and parse sections
detectAndParseSections :: Text -> [Section]
detectAndParseSections content
    | hasLatexSections content = parseLatexSections content
    | hasMarkdownSections content = parseMarkdownSections content
    | otherwise = [plainTextSection content]

-- | Check if content has LaTeX section commands
hasLatexSections :: Text -> Bool
hasLatexSections content =
    "\\section" `T.isInfixOf` content ||
    "\\subsection" `T.isInfixOf` content ||
    "\\chapter" `T.isInfixOf` content

-- | Check if content has Markdown headers
hasMarkdownSections :: Text -> Bool
hasMarkdownSections content =
    any (\line -> T.isPrefixOf "#" (T.stripStart line))
        (T.lines content)

-- | Create a single section for plain text
plainTextSection :: Text -> Section
plainTextSection content = Section
    { sectionId = SectionId "main"
    , sectionTitle = "Main"
    , sectionContent = content
    , sectionStartPos = 0
    , sectionEndPos = T.length content
    , sectionChildren = []
    }

--------------------------------------------------------------------------------
-- Source Operations
--------------------------------------------------------------------------------

-- | Get the content of a specific section
peekSection :: SourceDocument -> SectionId -> Either Text SourceContent
peekSection doc sid =
    case Map.lookup sid (sourceSections doc) of
        Nothing -> Left $ "Section not found: " <> unSectionId sid
        Just section ->
            Right SourceContent
                { sourceText = sectionContent section
                , sourceMath = extractMathExpressions (sectionContent section)
                }

-- | Search for a pattern in the source document
-- Uses simple text matching (not regex)
grepSource :: SourceDocument -> Pattern -> [Match]
grepSource doc pat =
    let patternText = unPattern pat
        content = sourceRawContent doc
        positions = findAllPositions content patternText 0
    in map (positionToMatch doc) positions
  where
    -- Find all occurrences of pattern in content
    findAllPositions :: Text -> Text -> Int -> [(Int, Int, Text)]
    findAllPositions content patternTxt offset
        | T.null patternTxt = []
        | otherwise =
            case T.breakOn patternTxt content of
                (before, rest)
                    | T.null rest -> []
                    | otherwise ->
                        let startPos = offset + T.length before
                            endPos = startPos + T.length patternTxt
                            snippet = extractSnippet (sourceRawContent doc) startPos endPos
                            remaining = T.drop (T.length patternTxt) rest
                        in (startPos, endPos, snippet) : findAllPositions remaining patternTxt endPos

    positionToMatch :: SourceDocument -> (Int, Int, Text) -> Match
    positionToMatch doc' (start, end, snippet) =
        Match
            { matchSection = findSectionForPosition doc' start
            , matchRange = (start, end)
            , matchSnippet = snippet
            }

-- | Get theorem content by ID
getTheorem :: SourceDocument -> TheoremId -> Either Text TheoremContent
getTheorem doc tid =
    case Map.lookup tid (sourceTheorems doc) of
        Nothing -> Left $ "Theorem not found: " <> unTheoremId tid
        Just thm ->
            Right TheoremContent
                { theoremStatement = thmStatement thm
                , theoremProof = thmProof thm
                }

-- | Get dependencies of a theorem
getDependencies :: SourceDocument -> TheoremId -> Either Text [TheoremId]
getDependencies doc tid =
    case Map.lookup tid (sourceTheorems doc) of
        Nothing -> Left $ "Theorem not found: " <> unTheoremId tid
        Just thm -> Right (thmDependencies thm)

--------------------------------------------------------------------------------
-- Parsing Utilities
--------------------------------------------------------------------------------

-- | Parse Markdown document into sections
parseMarkdownSections :: Text -> [Section]
parseMarkdownSections content =
    let linesWithPos = zip [0..] (T.lines content)
        headers = findMarkdownHeaders linesWithPos
        sections = buildSectionsFromHeaders content headers
    in if null sections
       then [plainTextSection content]
       else sections

-- | Find Markdown headers with their positions
findMarkdownHeaders :: [(Int, Text)] -> [(Int, Int, Text, Int)]  -- (lineNum, pos, title, level)
findMarkdownHeaders linesWithPos = mapMaybe parseHeader linesWithPos
  where
    parseHeader (lineNum, line) =
        let stripped = T.stripStart line
        in if T.isPrefixOf "#" stripped
           then let (hashes, rest) = T.span (== '#') stripped
                    level = T.length hashes
                    title = T.strip rest
                    -- Calculate character position
                    pos = sum [T.length l + 1 | (n, l) <- take lineNum linesWithPos]
                in Just (lineNum, pos, title, level)
           else Nothing

-- | Build sections from header information
buildSectionsFromHeaders :: Text -> [(Int, Int, Text, Int)] -> [Section]
buildSectionsFromHeaders content headers =
    let contentLen = T.length content
        indexed = zip [1..] headers
    in map (buildSection contentLen indexed) indexed
  where
    buildSection :: Int -> [(Int, (Int, Int, Text, Int))] -> (Int, (Int, Int, Text, Int)) -> Section
    buildSection contentLen indexed (idx, (_, pos, title, level)) =
        let endPos = findNextSectionStart indexed idx contentLen
            secId = SectionId $ T.pack $ show idx <> "." <> T.unpack (T.take 20 $ T.filter (/= ' ') title)
            secContent = T.take (endPos - pos) (T.drop pos content)
        in Section
            { sectionId = secId
            , sectionTitle = title
            , sectionContent = secContent
            , sectionStartPos = pos
            , sectionEndPos = endPos
            , sectionChildren = []  -- Could be computed from levels
            }

    findNextSectionStart :: [(Int, (Int, Int, Text, Int))] -> Int -> Int -> Int
    findNextSectionStart indexed currentIdx contentLen =
        case filter (\(i, _) -> i > currentIdx) indexed of
            [] -> contentLen
            ((_, (_, pos, _, _)):_) -> pos

-- | Parse LaTeX document into sections
parseLatexSections :: Text -> [Section]
parseLatexSections content =
    let commands = findLatexSectionCommands content
        sections = buildLatexSections content commands
    in if null sections
       then [plainTextSection content]
       else sections

-- | Find LaTeX section commands
findLatexSectionCommands :: Text -> [(Int, Text, Int)]  -- (pos, title, level)
findLatexSectionCommands content =
    let patterns = [ ("\\chapter{", 1)
                   , ("\\section{", 2)
                   , ("\\subsection{", 3)
                   , ("\\subsubsection{", 4)
                   ]
    in sortOn (\(p, _, _) -> p) $ concatMap (findCommand content) patterns
  where
    findCommand :: Text -> (Text, Int) -> [(Int, Text, Int)]
    findCommand txt (cmd, level) = findCommandPositions txt cmd level 0

    findCommandPositions :: Text -> Text -> Int -> Int -> [(Int, Text, Int)]
    findCommandPositions txt cmd level offset =
        case T.breakOn cmd txt of
            (before, rest)
                | T.null rest -> []
                | otherwise ->
                    let pos = offset + T.length before
                        afterCmd = T.drop (T.length cmd) rest
                        (title, remaining) = T.breakOn "}" afterCmd
                        nextOffset = pos + T.length cmd + T.length title + 1
                    in (pos, title, level) :
                       findCommandPositions (T.drop 1 remaining) cmd level nextOffset

-- | Build sections from LaTeX commands
buildLatexSections :: Text -> [(Int, Text, Int)] -> [Section]
buildLatexSections content commands =
    let contentLen = T.length content
        indexed = zip [1..] commands
    in map (buildSection contentLen indexed) indexed
  where
    buildSection :: Int -> [(Int, (Int, Text, Int))] -> (Int, (Int, Text, Int)) -> Section
    buildSection contentLen indexed (idx, (pos, title, _level)) =
        let endPos = case filter (\(i, _) -> i > idx) indexed of
                         [] -> contentLen
                         ((_, (p, _, _)):_) -> p
            secId = SectionId $ T.pack $ show idx <> "." <> T.take 20 (T.filter (/= ' ') title)
            secContent = T.take (endPos - pos) (T.drop pos content)
        in Section
            { sectionId = secId
            , sectionTitle = title
            , sectionContent = secContent
            , sectionStartPos = pos
            , sectionEndPos = endPos
            , sectionChildren = []
            }

-- | Extract mathematical expressions from text
extractMathExpressions :: Text -> [MathExpr]
extractMathExpressions content =
    extractInlineMath content ++ extractDisplayMath content

-- | Extract inline math ($...$)
extractInlineMath :: Text -> [MathExpr]
extractInlineMath content = findMathDelimited content "$" "$" 0

-- | Extract display math ($$...$$ or \[...\])
extractDisplayMath :: Text -> [MathExpr]
extractDisplayMath content =
    findMathDelimited content "$$" "$$" 0 ++
    findMathDelimited content "\\[" "\\]" 0

-- | Find math expressions between delimiters
findMathDelimited :: Text -> Text -> Text -> Int -> [MathExpr]
findMathDelimited content startDelim endDelim offset
    | T.null content = []
    | otherwise =
        case T.breakOn startDelim content of
            (before, rest)
                | T.null rest -> []
                | otherwise ->
                    let afterStart = T.drop (T.length startDelim) rest
                        startPos = offset + T.length before
                    in case T.breakOn endDelim afterStart of
                        (mathContent, afterEnd)
                            | T.null afterEnd -> []
                            | otherwise ->
                                let endPos = startPos + T.length startDelim + T.length mathContent + T.length endDelim
                                    remaining = T.drop (T.length endDelim) afterEnd
                                    expr = MathExpr
                                        { mathLatex = mathContent
                                        , mathLocation = (startPos, endPos)
                                        }
                                in expr : findMathDelimited remaining startDelim endDelim endPos

--------------------------------------------------------------------------------
-- Theorem Extraction
--------------------------------------------------------------------------------

-- | Extract theorems from document content
extractTheorems :: Text -> [Section] -> [Theorem]
extractTheorems content sections =
    concatMap (extractTheoremsFromSection content) sections

-- | Extract theorems from a single section
extractTheoremsFromSection :: Text -> Section -> [Theorem]
extractTheoremsFromSection _content section =
    let sectionText = sectionContent section
        -- Look for theorem-like environments
        thmPatterns = [ "Theorem", "Lemma", "Proposition", "Corollary"
                      , "Definition", "Example", "Remark"
                      ]
        found = concatMap (findTheoremPattern sectionText (sectionId section)) thmPatterns
    in found

-- | Find theorem patterns in text
findTheoremPattern :: Text -> SectionId -> Text -> [Theorem]
findTheoremPattern content sid pattern =
    findTheoremOccurrences content sid pattern 0 1
  where
    findTheoremOccurrences :: Text -> SectionId -> Text -> Int -> Int -> [Theorem]
    findTheoremOccurrences txt secId pat offset counter
        | T.null txt = []
        | otherwise =
            -- Look for patterns like "Theorem 1.2:" or "\begin{theorem}"
            let markers = [ pat <> " "
                          , "\\begin{" <> T.toLower pat <> "}"
                          ]
            in case findFirstMarker txt markers of
                Nothing -> []
                Just (markerPos, marker) ->
                    let pos = offset + markerPos
                        afterMarker = T.drop (markerPos + T.length marker) txt
                        (stmt, rest, proofText) = extractStatementAndProof afterMarker pat
                        thmIdent = TheoremId $ pat <> "-" <> unSectionId secId <> "-" <> T.pack (show counter)
                        deps = extractReferences stmt
                        thm = Theorem
                            { thmId = thmIdent
                            , thmSection = secId
                            , thmStatement = stmt
                            , thmProof = proofText
                            , thmDependencies = deps
                            , thmStartPos = pos
                            , thmEndPos = pos + T.length marker + T.length stmt + maybe 0 T.length proofText
                            }
                        newOffset = pos + T.length marker + T.length stmt + maybe 0 T.length proofText
                    in thm : findTheoremOccurrences rest secId pat newOffset (counter + 1)

    findFirstMarker :: Text -> [Text] -> Maybe (Int, Text)
    findFirstMarker txt markers =
        let positions = mapMaybe (findMarkerPos txt) markers
        in case sortOn fst positions of
               [] -> Nothing
               (p:_) -> Just p

    findMarkerPos :: Text -> Text -> Maybe (Int, Text)
    findMarkerPos txt marker =
        case T.breakOn marker txt of
            (before, rest)
                | T.null rest -> Nothing
                | otherwise -> Just (T.length before, marker)

-- | Extract theorem statement and optional proof
extractStatementAndProof :: Text -> Text -> (Text, Text, Maybe Text)
extractStatementAndProof content _thmType =
    -- Look for proof markers
    let proofMarkers = ["Proof.", "Proof:", "\\begin{proof}", "pf."]
        endMarkers = ["QED", "□", "\\end{proof}", "∎", "Theorem", "Lemma", "Proposition", "Definition"]
    in case findFirstOf content proofMarkers of
        Nothing ->
            -- No proof found, take until end marker or reasonable length
            let (stmt, rest) = takeUntilEndMarker content endMarkers
            in (T.strip stmt, rest, Nothing)
        Just (proofStart, _marker) ->
            let stmt = T.strip $ T.take proofStart content
                afterStmt = T.drop proofStart content
                (proof, rest) = takeUntilEndMarker afterStmt endMarkers
            in (stmt, rest, Just $ T.strip proof)
  where
    findFirstOf :: Text -> [Text] -> Maybe (Int, Text)
    findFirstOf txt markers =
        let positions = mapMaybe (findPos txt) markers
        in case sortOn fst positions of
               [] -> Nothing
               (p:_) -> Just p

    findPos :: Text -> Text -> Maybe (Int, Text)
    findPos txt marker =
        case T.breakOn marker txt of
            (before, rest)
                | T.null rest -> Nothing
                | otherwise -> Just (T.length before, marker)

    takeUntilEndMarker :: Text -> [Text] -> (Text, Text)
    takeUntilEndMarker txt markers =
        case findFirstOf txt markers of
            Nothing -> (T.take 2000 txt, T.drop 2000 txt)  -- Reasonable limit
            Just (pos, _) -> (T.take pos txt, T.drop pos txt)

-- | Extract theorem references from text (e.g., "by Theorem 2.1")
extractReferences :: Text -> [TheoremId]
extractReferences txt =
    let refPatterns = [ "Theorem ", "Lemma ", "Proposition ", "Corollary ", "Definition " ]
        refs = concatMap (findRefs txt) refPatterns
    in refs
  where
    findRefs :: Text -> Text -> [TheoremId]
    findRefs content pat = findRefsFrom content pat 0

    findRefsFrom :: Text -> Text -> Int -> [TheoremId]
    findRefsFrom content pat offset =
        case T.breakOn pat content of
            (_, rest)
                | T.null rest -> []
                | otherwise ->
                    let afterPat = T.drop (T.length pat) rest
                        refId = T.takeWhile (\c -> c `elem` ("0123456789." :: String)) afterPat
                        remaining = T.drop (T.length refId) afterPat
                    in if T.null refId
                       then findRefsFrom remaining pat (offset + T.length pat)
                       else TheoremId (T.init pat <> refId) : findRefsFrom remaining pat (offset + T.length pat + T.length refId)

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Find which section a position belongs to
findSectionForPosition :: SourceDocument -> Int -> SectionId
findSectionForPosition doc pos =
    let sections = Map.elems (sourceSections doc)
        containing = filter (\s -> sectionStartPos s <= pos && pos < sectionEndPos s) sections
    in case sortOn (negate . sectionStartPos) containing of  -- Most specific (deepest) first
           [] -> SectionId "unknown"
           (s:_) -> sectionId s

-- | Extract a snippet around a position
extractSnippet :: Text -> Int -> Int -> Text
extractSnippet content start end =
    let contextBefore = 30
        contextAfter = 30
        snippetStart = max 0 (start - contextBefore)
        snippetEnd = min (T.length content) (end + contextAfter)
        snippet = T.take (snippetEnd - snippetStart) (T.drop snippetStart content)
        prefix = if snippetStart > 0 then "..." else ""
        suffix = if snippetEnd < T.length content then "..." else ""
    in prefix <> snippet <> suffix
