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
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V

import qualified AgdaMCP.Types as Types

-- | Format a JSON response based on the requested format
formatResponse :: Types.ResponseFormat -> JSON.Value -> Text
formatResponse Types.Full jsonValue =
  -- Full format: return complete JSON
  TE.decodeUtf8 $ LBS.toStrict $ JSON.encode jsonValue

formatResponse Types.Concise jsonValue =
  -- Concise format: extract key information and format for humans
  case getField "kind" jsonValue of
    Just (JSON.String "DisplayInfo") -> formatDisplayInfo jsonValue
    Just (JSON.String "GiveAction") -> formatGiveAction jsonValue
    Just (JSON.String "MakeCase") -> formatMakeCase jsonValue
    _ -> "Response: " <> TE.decodeUtf8 (LBS.toStrict $ JSON.encode jsonValue)

-- | Extract format parameter from tool (default to Concise)
-- Parse the Maybe Text field to ResponseFormat
getFormat :: Types.AgdaTool -> Types.ResponseFormat
getFormat tool =
  let formatText = case tool of
        Types.AgdaLoad{Types.format=fmt} -> fmt
        Types.AgdaGetGoals{Types.format=fmt} -> fmt
        Types.AgdaGetGoalType{Types.format=fmt} -> fmt
        Types.AgdaGetContext{Types.format=fmt} -> fmt
        Types.AgdaGive{Types.format=fmt} -> fmt
        Types.AgdaRefine{Types.format=fmt} -> fmt
        Types.AgdaCaseSplit{Types.format=fmt} -> fmt
        Types.AgdaCompute{Types.format=fmt} -> fmt
        Types.AgdaInferType{Types.format=fmt} -> fmt
        Types.AgdaIntro{Types.format=fmt} -> fmt
        Types.AgdaWhyInScope{Types.format=fmt} -> fmt
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
  let name = case getField "name" entry of
        Just (JSON.String n) -> n
        _ -> "?"
      typ = case getField "type" entry of
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
