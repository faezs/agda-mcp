{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Format
  ( formatResponse
  , getFormat
  ) where

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Key as JSON.Key
import qualified Data.Aeson.KeyMap as JSON.KeyMap
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as TE
import Data.Text (Text)
import qualified Data.Vector as V

import qualified AgdaMCP.Types as Types

-- | Format a JSON response based on the requested format
formatResponse :: Types.ResponseFormat -> JSON.Value -> Text
formatResponse Types.Full jsonValue =
  -- Full format: return complete JSON
  TE.decodeUtf8 $ LBS.toStrict $ JSON.encode jsonValue

formatResponse Types.Concise jsonValue =
  -- Concise format: extract key information and format for humans
  case jsonValue of
    -- Check for postulates list (JSON array with pName/pType/pRange fields)
    JSON.Array arr | not (V.null arr) && isPostulateArray arr ->
      formatPostulates jsonValue
    -- Check for single goal object (goalId, goalType, range fields)
    JSON.Object obj | isSingleGoalObject obj ->
      formatSingleGoal jsonValue
    -- Check for standard Agda response with "kind" field
    _ -> case getField "kind" jsonValue of
      Just (JSON.String "DisplayInfo") -> formatDisplayInfo jsonValue
      Just (JSON.String "GiveAction") -> formatGiveAction jsonValue
      Just (JSON.String "MakeCase") -> formatMakeCase jsonValue
      Just (JSON.String "SolveAll") -> formatSolveAll jsonValue
      _ -> "Response: " <> TE.decodeUtf8 (LBS.toStrict $ JSON.encode jsonValue)

-- | Extract format parameter from tool (default to Concise)
-- Parse the Maybe Text field to ResponseFormat
getFormat :: Types.AgdaTool -> Types.ResponseFormat
getFormat tool =
  let formatText = case tool of
        Types.AgdaLoad{Types.format=fmt} -> fmt
        Types.AgdaGetGoals{Types.format=fmt} -> fmt
        Types.AgdaGetGoalType{Types.format=fmt} -> fmt
        Types.AgdaGetGoalTypeImplicits{Types.format=fmt} -> fmt
        Types.AgdaGetContext{Types.format=fmt} -> fmt
        Types.AgdaGetContextImplicits{Types.format=fmt} -> fmt
        Types.AgdaGive{Types.format=fmt} -> fmt
        Types.AgdaRefine{Types.format=fmt} -> fmt
        Types.AgdaCaseSplit{Types.format=fmt} -> fmt
        Types.AgdaCompute{Types.format=fmt} -> fmt
        Types.AgdaInferType{Types.format=fmt} -> fmt
        Types.AgdaIntro{Types.format=fmt} -> fmt
        Types.AgdaAuto{Types.format=fmt} -> fmt
        Types.AgdaAutoAll{Types.format=fmt} -> fmt
        Types.AgdaSolveOne{Types.format=fmt} -> fmt
        Types.AgdaHelperFunction{Types.format=fmt} -> fmt
        Types.AgdaGoalTypeContext{Types.format=fmt} -> fmt
        Types.AgdaGoalAtPosition{Types.format=fmt} -> fmt
        Types.AgdaGotoDefinition{Types.format=fmt} -> fmt
        Types.AgdaSearchAbout{Types.format=fmt} -> fmt
        Types.AgdaShowModule{Types.format=fmt} -> fmt
        Types.AgdaShowConstraints{Types.format=fmt} -> fmt
        Types.AgdaWhyInScope{Types.format=fmt} -> fmt
        Types.AgdaListPostulates{Types.format=fmt} -> fmt
  in parseFormat formatText

-- | Parse format string to ResponseFormat (default: Concise)
parseFormat :: Maybe Text -> Types.ResponseFormat
parseFormat Nothing = Types.Concise
parseFormat (Just "Full") = Types.Full
parseFormat (Just "full") = Types.Full
parseFormat (Just "Concise") = Types.Concise
parseFormat (Just "concise") = Types.Concise
parseFormat _ = Types.Concise  -- Default to Concise for unknown values

-- ============================================================================
-- DisplayInfo Formatting (most common response type)
-- ============================================================================

formatDisplayInfo :: JSON.Value -> Text
formatDisplayInfo jsonValue =
  case getField "info" jsonValue of
    Just infoValue -> case getField "kind" infoValue of
      Just (JSON.String "AllGoalsWarnings") -> formatAllGoalsWarnings infoValue
      Just (JSON.String "Error") -> formatError infoValue
      Just (JSON.String "InferredType") -> formatInferredType infoValue
      Just (JSON.String "NormalForm") -> formatNormalForm infoValue
      Just (JSON.String "Context") -> formatContext infoValue
      Just (JSON.String "GoalType") -> formatGoalType infoValue
      Just (JSON.String "WhyInScope") -> formatWhyInScope infoValue
      Just (JSON.String "Intro") -> formatIntro infoValue
      Just (JSON.String "Auto") -> formatAuto infoValue
      Just (JSON.String "SearchAbout") -> formatSearchAbout infoValue
      Just (JSON.String "ModuleContents") -> formatModuleContents infoValue
      Just (JSON.String "Constraints") -> formatConstraints infoValue
      _ -> "DisplayInfo: " <> extractText infoValue
    _ -> "DisplayInfo (no info)"

-- Format AllGoalsWarnings (from load/get_goals)
formatAllGoalsWarnings :: JSON.Value -> Text
formatAllGoalsWarnings infoValue =
  let visibleGoals = getField "visibleGoals" infoValue
      goalCount = case visibleGoals of
        Just (JSON.Array arr) -> V.length arr
        _ -> 0
      goalsSummary = case visibleGoals of
        Just (JSON.Array arr) -> T.intercalate " " $ V.toList $ V.map formatGoalSummary arr
        _ -> ""
  in T.pack (show goalCount) <> " goals: " <> goalsSummary

-- Format a single goal as "?0:Nat(10:12)"
formatGoalSummary :: JSON.Value -> Text
formatGoalSummary goalValue =
  let goalId = case getField "constraintObj" goalValue >>= getField "id" of
        Just (JSON.Number n) -> T.pack $ show (floor n :: Int)
        _ -> "?"
      goalType = case getField "type" goalValue of
        Just (JSON.String t) -> t
        _ -> "?"
      range = case getField "constraintObj" goalValue >>= getField "range" of
        Just (JSON.Array arr) | V.length arr > 0 ->
          case V.head arr of
            JSON.Object obj ->
              let line = case JSON.KeyMap.lookup "line" obj of
                    Just (JSON.Number n) -> T.pack $ show (floor n :: Int)
                    _ -> "?"
                  col = case JSON.KeyMap.lookup "col" obj of
                    Just (JSON.Number n) -> T.pack $ show (floor n :: Int)
                    _ -> "?"
              in "(" <> line <> ":" <> col <> ")"
            _ -> ""
        _ -> ""
  in "?" <> goalId <> ":" <> goalType <> range

-- Format error message
formatError :: JSON.Value -> Text
formatError infoValue =
  case getField "error" infoValue >>= getField "message" of
    Just (JSON.String msg) -> "Error: " <> msg
    _ -> "Error (no message)"

-- Format inferred type
formatInferredType :: JSON.Value -> Text
formatInferredType infoValue =
  case getField "expr" infoValue of
    Just (JSON.String expr) -> case getField "type" infoValue of
      Just (JSON.String typ) -> expr <> " : " <> typ
      _ -> "Type: " <> expr
    _ -> "InferredType"

-- Format normal form
formatNormalForm :: JSON.Value -> Text
formatNormalForm infoValue =
  case getField "expr" infoValue of
    Just (JSON.String expr) -> expr
    _ -> extractText infoValue

-- Format context
formatContext :: JSON.Value -> Text
formatContext infoValue =
  case getField "context" infoValue of
    Just (JSON.Array arr) ->
      let contextLines = V.toList $ V.map formatContextEntry arr
      in if null contextLines
           then "Context: (empty)"
           else "Context:\n  " <> T.intercalate "\n  " contextLines
    _ -> "Context: " <> extractText infoValue

formatContextEntry :: JSON.Value -> Text
formatContextEntry entry =
  let name = case getField "originalName" entry of
        Just (JSON.String n) -> n
        Just _ -> "?"
        Nothing -> case getField "reifiedName" entry of
          Just (JSON.String n) -> n
          _ -> "?"
      typ = case getField "binding" entry of
        Just (JSON.String t) -> t
        _ -> "?"
  in name <> " : " <> typ

-- Format goal type
formatGoalType :: JSON.Value -> Text
formatGoalType infoValue =
  case getField "goalId" infoValue of
    Just (JSON.Number gid) ->
      let goalIdText = T.pack $ show (floor gid :: Int)
      in case getField "type" infoValue of
        Just (JSON.String typ) -> "?" <> goalIdText <> " : " <> typ
        _ -> "Goal ?" <> goalIdText
    _ -> extractText infoValue

-- Format why in scope
formatWhyInScope :: JSON.Value -> Text
formatWhyInScope infoValue =
  extractText infoValue

-- Format intro suggestions
formatIntro :: JSON.Value -> Text
formatIntro infoValue =
  "Intro: " <> extractText infoValue

-- Format auto proof search results
formatAuto :: JSON.Value -> Text
formatAuto infoValue =
  case getField "autoResult" infoValue of
    Just (JSON.String result) -> "Auto: " <> result
    _ -> "Auto: " <> extractText infoValue

-- Format search about results (type search)
formatSearchAbout :: JSON.Value -> Text
formatSearchAbout infoValue =
  case getField "results" infoValue of
    Just (JSON.Array results) ->
      if V.null results
        then "No results found"
        else
          let count = V.length results
              formatted = V.toList $ V.map formatSearchResult results
          in T.pack (show count) <> " results:\n  " <> T.intercalate "\n  " formatted
    _ -> "SearchAbout: " <> extractText infoValue
  where
    formatSearchResult :: JSON.Value -> Text
    formatSearchResult result =
      case (getField "name" result, getField "type" result) of
        (Just (JSON.String n), Just (JSON.String t)) -> n <> " : " <> t
        _ -> extractText result

-- Format module contents (definitions, types, submodules)
formatModuleContents :: JSON.Value -> Text
formatModuleContents infoValue =
  case getField "contents" infoValue of
    Just (JSON.Array contents) ->
      if V.null contents
        then "Module is empty"
        else
          let count = V.length contents
              formatted = V.toList $ V.map formatModuleEntry contents
          in T.pack (show count) <> " definitions:\n  " <> T.intercalate "\n  " formatted
    _ -> "ModuleContents: " <> extractText infoValue
  where
    formatModuleEntry :: JSON.Value -> Text
    formatModuleEntry entry =
      case (getField "name" entry, getField "type" entry) of
        (Just (JSON.String n), Just (JSON.String t)) -> n <> " : " <> t
        (Just (JSON.String n), Nothing) -> n  -- Submodule without type
        _ -> extractText entry

-- Format constraints (unsolved type-checking constraints)
formatConstraints :: JSON.Value -> Text
formatConstraints infoValue =
  case getField "constraints" infoValue of
    Just (JSON.Array constraints) ->
      if V.null constraints
        then "No constraints"
        else
          let count = V.length constraints
              formatted = V.toList $ V.map formatConstraint constraints
          in T.pack (show count) <> " constraints:\n  " <> T.intercalate "\n  " formatted
    _ -> "Constraints: " <> extractText infoValue
  where
    formatConstraint :: JSON.Value -> Text
    formatConstraint constraint =
      case getField "constraint" constraint of
        Just (JSON.String c) -> c
        _ -> extractText constraint

-- ============================================================================
-- GiveAction Formatting
-- ============================================================================

formatGiveAction :: JSON.Value -> Text
formatGiveAction jsonValue =
  let goalId = case getField "interactionPoint" jsonValue >>= getField "id" of
        Just (JSON.Number n) -> T.pack $ show (floor n :: Int)
        _ -> "?"
      result = case getField "giveResult" jsonValue >>= getField "str" of
        Just (JSON.String s) -> s
        _ -> "(no result)"
  in "✓ Filled ?" <> goalId <> " with '" <> result <> "'"

-- ============================================================================
-- MakeCase Formatting
-- ============================================================================

formatMakeCase :: JSON.Value -> Text
formatMakeCase jsonValue =
  let goalId = case getField "interactionPoint" jsonValue >>= getField "id" of
        Just (JSON.Number n) -> T.pack $ show (floor n :: Int)
        _ -> "?"
      clauses = case getField "clauses" jsonValue of
        Just (JSON.Array arr) ->
          let clauseTexts = V.toList $ V.mapMaybe extractString arr
          in if null clauseTexts
               then []
               else clauseTexts
        _ -> []
      clauseCount = length clauses
  in "Split ?" <> goalId <> " into " <> T.pack (show clauseCount) <> " clauses:\n  " <>
     T.intercalate "\n  " clauses

-- ============================================================================
-- SolveAll Formatting
-- ============================================================================

formatSolveAll :: JSON.Value -> Text
formatSolveAll jsonValue =
  case getField "solutions" jsonValue of
    Just (JSON.Array solutions) ->
      if V.null solutions
        then "No goals solved"
        else
          let count = V.length solutions
              formatted = V.toList $ V.map formatSolution solutions
          in "✓ Solved " <> T.pack (show count) <> " goals:\n  " <> T.intercalate "\n  " formatted
    _ -> "SolveAll: " <> extractText jsonValue
  where
    formatSolution :: JSON.Value -> Text
    formatSolution sol =
      case (getField "goalId" sol, getField "expression" sol) of
        (Just (JSON.Number gid), Just (JSON.String expr)) ->
          "?" <> T.pack (show (floor gid :: Int)) <> " := " <> expr
        _ -> extractText sol

-- ============================================================================
-- Single Goal Formatting
-- ============================================================================

-- Check if object is a single goal (has goalId, goalType, range fields)
isSingleGoalObject :: JSON.KeyMap.KeyMap JSON.Value -> Bool
isSingleGoalObject obj =
  let hasGoalId = JSON.KeyMap.member (JSON.Key.fromText "goalId") obj
      hasGoalType = JSON.KeyMap.member (JSON.Key.fromText "goalType") obj
      hasRange = JSON.KeyMap.member (JSON.Key.fromText "range") obj
  in hasGoalId && hasGoalType && hasRange

formatSingleGoal :: JSON.Value -> Text
formatSingleGoal jsonValue =
  let goalId = case getField "goalId" jsonValue of
        Just (JSON.Number n) -> T.pack $ show (floor n :: Int)
        _ -> "?"
      goalType = case getField "goalType" jsonValue of
        Just (JSON.String t) -> t
        _ -> "?"
      range = case getField "range" jsonValue of
        Just (JSON.Array arr) | V.length arr >= 2 ->
          case (arr V.! 0, arr V.! 1) of
            (JSON.Number line, JSON.Number col) ->
              "(" <> T.pack (show (floor line :: Int)) <> ":" <> T.pack (show (floor col :: Int)) <> ")"
            _ -> ""
        _ -> ""
  in "?<" <> goalId <> "> : " <> goalType <> " " <> range

-- ============================================================================
-- Postulates Formatting
-- ============================================================================

-- Check if array contains postulate objects (has pName, pType, pRange fields)
isPostulateArray :: V.Vector JSON.Value -> Bool
isPostulateArray arr =
  case V.headM arr of
    Nothing -> False
    Just firstElem -> case firstElem of
      JSON.Object obj ->
        let hasPName = JSON.KeyMap.member (JSON.Key.fromText "pName") obj
            hasPType = JSON.KeyMap.member (JSON.Key.fromText "pType") obj
            hasPRange = JSON.KeyMap.member (JSON.Key.fromText "pRange") obj
        in hasPName && hasPType && hasPRange
      _ -> False

formatPostulates :: JSON.Value -> Text
formatPostulates (JSON.Array arr) =
  let postulates = V.toList arr
      count = length postulates
      names = V.toList $ V.mapMaybe extractPostulateName arr
      lines' = V.toList $ V.mapMaybe extractPostulateLine arr
  in if count == 0
       then "No postulates found"
       else T.pack (show count) <> " postulates: " <>
            T.intercalate ", " names <>
            " at lines " <>
            T.intercalate ", " (map (T.pack . show) lines')
formatPostulates _ = "Invalid postulates format"

-- Extract postulate name from JSON object
extractPostulateName :: JSON.Value -> Maybe Text
extractPostulateName val = getField "pName" val >>= extractString

-- Extract postulate line number from JSON object
extractPostulateLine :: JSON.Value -> Maybe Int
extractPostulateLine val = do
  pRange <- getField "pRange" val
  case pRange of
    JSON.Array rangeArr | V.length rangeArr >= 1 ->
      case rangeArr V.! 0 of
        JSON.Number n -> Just (floor n :: Int)
        _ -> Nothing
    _ -> Nothing

-- ============================================================================
-- Utility Functions
-- ============================================================================

-- Get field from JSON object
getField :: Text -> JSON.Value -> Maybe JSON.Value
getField field (JSON.Object obj) = JSON.KeyMap.lookup (JSON.Key.fromText field) obj
getField _ _ = Nothing

-- Extract text from various JSON values
extractText :: JSON.Value -> Text
extractText (JSON.String t) = t
extractText (JSON.Number n) = T.pack $ show n
extractText (JSON.Bool b) = if b then "true" else "false"
extractText JSON.Null = "null"
extractText v = TE.decodeUtf8 $ LBS.toStrict $ JSON.encode v

-- Extract string from JSON value
extractString :: JSON.Value -> Maybe Text
extractString (JSON.String t) = Just t
extractString _ = Nothing
