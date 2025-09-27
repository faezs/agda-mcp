{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module AgdaMCP.Server where

import qualified Data.Aeson as JSON
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import GHC.Generics (Generic)
import Control.Monad.IO.Class (liftIO)
import System.FilePath (takeDirectory)
import System.Directory (setCurrentDirectory, getCurrentDirectory)

-- Agda imports
import Agda.Interaction.Base
import Agda.Interaction.FindFile (SourceFile(..))
import Agda.Interaction.Imports (Mode(..), CheckResult(..), crInterface)
import Agda.Interaction.Response (InteractionOutputCallback, Response(..))
import Agda.TypeChecking.Monad (TCM, runTCM, TCState, initState, stInteractionPoints)
import Agda.TypeChecking.Monad.Base (InteractionPoint(..), InteractionId(..), ipRange, ipMeta)
import Agda.TypeChecking.Pretty (prettyTCM)
import Agda.TypeChecking.Rules.Term (checkExpr)
import Agda.Utils.FileName (absolute)
import Agda.Utils.Pretty (render)

-- Data structures for MCP communication
data AgdaGoal = AgdaGoal
  { goalId :: Int
  , goalType :: Text
  , goalRange :: (Int, Int, Int, Int)  -- startLine, startCol, endLine, endCol
  } deriving (Generic, Show)

instance JSON.ToJSON AgdaGoal
instance JSON.FromJSON AgdaGoal

data AgdaResult = AgdaResult
  { success :: Bool
  , message :: Text
  , result :: Maybe JSON.Value
  } deriving (Generic, Show)

instance JSON.ToJSON AgdaResult
instance JSON.FromJSON AgdaResult

-- MCP Server entry point
runAgdaMCPServer :: IO ()
runAgdaMCPServer = do
  -- Initialize MCP server with our tool handlers
  -- This would use the mcp-server library to handle the protocol
  -- For now, we'll implement a simple JSON interface
  putStrLn "Agda MCP Server started. Send JSON commands to stdin."
  handleMCPCommands

-- Simple command handler (would be replaced with proper MCP protocol)
handleMCPCommands :: IO ()
handleMCPCommands = do
  line <- getLine
  case JSON.decode (T.encodeUtf8 $ T.pack line) of
    Just cmd -> do
      result <- processCommand cmd
      TIO.putStrLn (T.decodeUtf8 $ JSON.encode result)
      handleMCPCommands
    Nothing -> do
      putStrLn "Invalid JSON command"
      handleMCPCommands

processCommand :: JSON.Value -> IO AgdaResult
processCommand = \case
  JSON.Object obj -> case JSON.lookup "command" obj of
    Just (JSON.String "get_goals") ->
      case JSON.lookup "file" obj of
        Just (JSON.String file) -> getGoals (T.unpack file)
        _ -> pure $ AgdaResult False "Missing file parameter" Nothing
    Just (JSON.String "get_goal_type") ->
      case (JSON.lookup "file" obj, JSON.lookup "goal_id" obj) of
        (Just (JSON.String file), Just (JSON.Number goalNum)) ->
          getGoalType (T.unpack file) (floor goalNum)
        _ -> pure $ AgdaResult False "Missing file or goal_id parameter" Nothing
    _ -> pure $ AgdaResult False "Unknown command" Nothing
  _ -> pure $ AgdaResult False "Invalid command format" Nothing

-- Core Agda interaction functions
getGoals :: FilePath -> IO AgdaResult
getGoals filePath = do
  originalDir <- getCurrentDirectory
  setCurrentDirectory (takeDirectory filePath)

  result <- runTCM $ do
    -- Load the file
    sourceFile <- liftIO $ SourceFile <$> absolute filePath
    checkResult <- typeCheckMain sourceFile

    -- Get interaction points (goals)
    state <- get
    let interactionPoints = stInteractionPoints state

    goals <- mapM interactionPointToGoal (Map.toList interactionPoints)
    return goals

  setCurrentDirectory originalDir

  case result of
    Right goals -> pure $ AgdaResult True "Success" (Just $ JSON.toJSON goals)
    Left err -> pure $ AgdaResult False (T.pack $ show err) Nothing

getGoalType :: FilePath -> Int -> IO AgdaResult
getGoalType filePath goalId = do
  originalDir <- getCurrentDirectory
  setCurrentDirectory (takeDirectory filePath)

  result <- runTCM $ do
    -- Load the file
    sourceFile <- liftIO $ SourceFile <$> absolute filePath
    checkResult <- typeCheckMain sourceFile

    -- Get the specific goal type
    state <- get
    let interactionPoints = stInteractionPoints state
    case Map.lookup (InteractionId goalId) interactionPoints of
      Just ip -> do
        goalType <- prettyTCM =<< getInteractionPointType ip
        return $ render goalType
      Nothing -> return "Goal not found"

  setCurrentDirectory originalDir

  case result of
    Right goalType -> pure $ AgdaResult True "Success" (Just $ JSON.String $ T.pack goalType)
    Left err -> pure $ AgdaResult False (T.pack $ show err) Nothing

-- Helper functions
interactionPointToGoal :: (InteractionId, InteractionPoint) -> TCM AgdaGoal
interactionPointToGoal (InteractionId iid, ip) = do
  let range = ipRange ip
  goalType <- prettyTCM =<< getInteractionPointType ip
  return $ AgdaGoal
    { goalId = iid
    , goalType = T.pack (render goalType)
    , goalRange = rangeToTuple range
    }

rangeToTuple :: Range -> (Int, Int, Int, Int)
rangeToTuple range =
  -- Extract line/column info from Agda's Range type
  -- This would need proper implementation based on Range structure
  (1, 1, 1, 10)  -- placeholder

getInteractionPointType :: InteractionPoint -> TCM Type
getInteractionPointType ip = do
  -- Get the type of the goal at this interaction point
  -- This would use Agda's internal functions to extract the goal type
  -- Placeholder implementation
  return undefined

typeCheckMain :: SourceFile -> TCM CheckResult
typeCheckMain sourceFile = do
  -- Use Agda's type checking pipeline
  -- This would call the appropriate Agda functions to load and check the file
  -- Placeholder implementation
  return undefined