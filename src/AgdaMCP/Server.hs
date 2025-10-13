{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module AgdaMCP.Server
  ( ServerState(..)
  , initServerState
  , handleAgdaTool
  , handleAgdaResource
  ) where

import qualified Data.Aeson as JSON
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Data.Maybe (fromMaybe, isNothing)
import Data.IORef
import GHC.Generics (Generic)
import Control.Monad (forM, when)
import Control.Monad.IO.Class (liftIO)

-- MCP Server library
import qualified MCP.Server
import qualified AgdaMCP.Types

-- Agda imports - using the actual interaction functions
import Agda.Interaction.Base
import Agda.Interaction.BasicOps as BasicOps
import Agda.Interaction.InteractionTop (cmd_load')
import Agda.Interaction.MakeCase (makeCase)
import Agda.Interaction.Imports (CheckResult, crInterface, Mode(..), parseSource, typeCheckMain)
import Agda.Interaction.Output (OutputConstraint)

import Agda.Syntax.Common
import Agda.Syntax.Common.Pretty (render, pretty, prettyShow)
import Agda.Syntax.Position (Range, rStart, rEnd, posLine, posCol, noRange)
import Agda.Syntax.Scope.Base (WhyInScopeData(..))
import Agda.Syntax.Abstract (Expr)

import Agda.TypeChecking.Monad
import Agda.TypeChecking.Monad.Base (iInsideScope, SourceFile(..) )
import Agda.TypeChecking.Monad.MetaVars (getInteractionPoints, getInteractionRange, withInteractionId)
import Agda.TypeChecking.Pretty (prettyTCM, prettyA)

import Agda.Utils.FileName (absolute, AbsolutePath, filePath)
import Agda.Utils.FileId (FileId(..))

-- Data structures for MCP communication
data AgdaGoal = AgdaGoal
  { goalId :: Int
  , goalType :: Text
  , goalRange :: (Int, Int, Int, Int)
  } deriving (Generic, Show)

instance JSON.ToJSON AgdaGoal
instance JSON.FromJSON AgdaGoal

data AgdaContextEntry = AgdaContextEntry
  { entryName :: Text
  , entryType :: Text
  , entryValue :: Maybe Text
  } deriving (Generic, Show)

instance JSON.ToJSON AgdaContextEntry
instance JSON.FromJSON AgdaContextEntry

data AgdaResult = AgdaResult
  { success :: Bool
  , message :: Text
  , agdaResult :: Maybe JSON.Value
  } deriving (Generic, Show)

instance JSON.ToJSON AgdaResult
instance JSON.FromJSON AgdaResult

-- Server state - just track the current file and its check result
data ServerState = ServerState
  { currentFile :: Maybe AbsolutePath
  , checkResult :: Maybe CheckResult
  }

-- MCP Server entry point
runAgdaMCPServer :: IO ()
runAgdaMCPServer = do
  putStrLn "Agda MCP Server started. Send JSON commands to stdin."
  stateRef <- newIORef (ServerState Nothing Nothing)
  serverLoop stateRef

-- Server loop
serverLoop :: IORef ServerState -> IO ()
serverLoop stateRef = do
  line <- getLine
  case JSON.decode (LBS.fromStrict $ TE.encodeUtf8 $ T.pack line) of
    Nothing -> do
      let errorResult = AgdaResult False "Invalid JSON command" Nothing
      TIO.putStrLn (TE.decodeUtf8 $ LBS.toStrict $ JSON.encode errorResult)
      serverLoop stateRef
    Just cmd -> do
      result <- runTCMTop $ processMCPCommand stateRef cmd
      case result of
        Left err -> do
          let errorResult = AgdaResult False (T.pack $ show err) Nothing
          TIO.putStrLn (TE.decodeUtf8 $ LBS.toStrict $ JSON.encode errorResult)
        Right res ->
          TIO.putStrLn (TE.decodeUtf8 $ LBS.toStrict $ JSON.encode res)
      serverLoop stateRef

-- Process MCP commands
processMCPCommand :: IORef ServerState -> JSON.Value -> TCM AgdaResult
processMCPCommand stateRef = \case
  JSON.Object obj -> case KM.lookup "command" obj of
    Just (JSON.String "load") ->
      case KM.lookup "file" obj of
        Just (JSON.String file) -> mcpLoadFile stateRef (T.unpack file)
        _ -> pure $ AgdaResult False "Missing file parameter" Nothing

    Just (JSON.String "get_goals") ->
      mcpGetGoals stateRef

    Just (JSON.String "get_goal_type") ->
      case KM.lookup "goal_id" obj of
        Just (JSON.Number goalNum) -> mcpGetGoalType stateRef (floor goalNum)
        _ -> pure $ AgdaResult False "Missing goal_id parameter" Nothing

    Just (JSON.String "get_context") ->
      case KM.lookup "goal_id" obj of
        Just (JSON.Number goalNum) -> mcpGetContext stateRef (floor goalNum)
        _ -> pure $ AgdaResult False "Missing goal_id parameter" Nothing

    Just (JSON.String "give") ->
      case (KM.lookup "goal_id" obj, KM.lookup "expression" obj) of
        (Just (JSON.Number goalNum), Just (JSON.String expr)) ->
          mcpGive stateRef (floor goalNum) (T.unpack expr)
        _ -> pure $ AgdaResult False "Missing goal_id or expression parameter" Nothing

    Just (JSON.String "refine") ->
      case (KM.lookup "goal_id" obj, KM.lookup "expression" obj) of
        (Just (JSON.Number goalNum), Just (JSON.String expr)) ->
          mcpRefine stateRef (floor goalNum) (T.unpack expr)
        _ -> pure $ AgdaResult False "Missing goal_id or expression parameter" Nothing

    Just (JSON.String "case_split") ->
      case (KM.lookup "goal_id" obj, KM.lookup "variable" obj) of
        (Just (JSON.Number goalNum), Just (JSON.String var)) ->
          mcpCaseSplit stateRef (floor goalNum) (T.unpack var)
        _ -> pure $ AgdaResult False "Missing goal_id or variable parameter" Nothing

    Just (JSON.String "compute") ->
      case (KM.lookup "goal_id" obj, KM.lookup "expression" obj) of
        (Just (JSON.Number goalNum), Just (JSON.String expr)) ->
          mcpCompute stateRef (floor goalNum) (T.unpack expr)
        _ -> pure $ AgdaResult False "Missing goal_id or expression parameter" Nothing

    Just (JSON.String "infer_type") ->
      case (KM.lookup "goal_id" obj, KM.lookup "expression" obj) of
        (Just (JSON.Number goalNum), Just (JSON.String expr)) ->
          mcpInferType stateRef (floor goalNum) (T.unpack expr)
        _ -> pure $ AgdaResult False "Missing goal_id or expression parameter" Nothing

    Just (JSON.String "intro") ->
      case KM.lookup "goal_id" obj of
        Just (JSON.Number goalNum) -> mcpIntro stateRef (floor goalNum)
        _ -> pure $ AgdaResult False "Missing goal_id parameter" Nothing

    Just (JSON.String "why_in_scope") ->
      case KM.lookup "name" obj of
        Just (JSON.String name) -> mcpWhyInScope stateRef (T.unpack name)
        _ -> pure $ AgdaResult False "Missing name parameter" Nothing

    _ -> pure $ AgdaResult False "Unknown command" Nothing
  _ -> pure $ AgdaResult False "Invalid command format" Nothing

-- Command implementations

mcpLoadFile :: IORef ServerState -> FilePath -> TCM AgdaResult
mcpLoadFile stateRef file = do
  absPath <- liftIO $ absolute file
  -- Use a simple FileId (Word32)- just use 0 for now, Agda will handle it
  let sourceFile = SourceFile (FileId 0)
  source <- parseSource sourceFile
  checked <- typeCheckMain TypeCheck source
  setScope $ iInsideScope $ crInterface checked
  liftIO $ writeIORef stateRef (ServerState (Just absPath) (Just checked))
  pure $ AgdaResult True "File loaded successfully" Nothing

mcpGetGoals :: IORef ServerState -> TCM AgdaResult
mcpGetGoals stateRef = do
  state <- liftIO $ readIORef stateRef
  case currentFile state of
    Nothing -> pure $ AgdaResult False "No file loaded" Nothing
    Just _ -> do
      interactionIds <- getInteractionPoints
      goals <- forM interactionIds $ \iid -> do
        range <- getInteractionRange iid
        constraint <- BasicOps.typeOfMeta AsIs iid
        goalTypeText <- renderConstraintType constraint
        return $ AgdaGoal
          { goalId = fromIntegral ((\(InteractionId n) -> n) iid)
          , goalType = goalTypeText
          , goalRange = rangeToTuple range
          }
      pure $ AgdaResult True "Success" (Just $ JSON.toJSON goals)

mcpGetGoalType :: IORef ServerState -> Int -> TCM AgdaResult
mcpGetGoalType stateRef goalIdParam = do
  state <- liftIO $ readIORef stateRef
  case currentFile state of
    Nothing -> pure $ AgdaResult False "No file loaded" Nothing
    Just _ -> do
      let iid = InteractionId goalIdParam
      constraint <- withInteractionId iid $ BasicOps.typeOfMeta AsIs iid
      goalTypeText <- renderConstraintType constraint
      pure $ AgdaResult True "Success" (Just $ JSON.String goalTypeText)

mcpGetContext :: IORef ServerState -> Int -> TCM AgdaResult
mcpGetContext stateRef goalIdParam = do
  state <- liftIO $ readIORef stateRef
  case currentFile state of
    Nothing -> pure $ AgdaResult False "No file loaded" Nothing
    Just _ -> do
      let iid = InteractionId goalIdParam
      constraint <- withInteractionId iid $ BasicOps.typeOfMeta AsIs iid
      contextEntries <- renderConstraintContext constraint
      pure $ AgdaResult True "Success" (Just $ JSON.toJSON contextEntries)

mcpGive :: IORef ServerState -> Int -> String -> TCM AgdaResult
mcpGive stateRef goalIdParam exprStr = do
  state <- liftIO $ readIORef stateRef
  case currentFile state of
    Nothing -> pure $ AgdaResult False "No file loaded" Nothing
    Just _ -> do
      let iid = InteractionId goalIdParam
      expr <- BasicOps.parseExprIn iid noRange exprStr
      result <- withInteractionId iid $ BasicOps.give WithoutForce iid Nothing expr
      resultDoc <- prettyTCM result
      let resultText = render resultDoc
      pure $ AgdaResult True "Expression given" (Just $ JSON.String $ T.pack resultText)

mcpRefine :: IORef ServerState -> Int -> String -> TCM AgdaResult
mcpRefine stateRef goalIdParam exprStr = do
  state <- liftIO $ readIORef stateRef
  case currentFile state of
    Nothing -> pure $ AgdaResult False "No file loaded" Nothing
    Just _ -> do
      let iid = InteractionId goalIdParam
      expr <- BasicOps.parseExprIn iid noRange exprStr
      result <- withInteractionId iid $ BasicOps.refine WithoutForce iid Nothing expr
      resultDoc <- prettyTCM result
      let resultText = render resultDoc
      pure $ AgdaResult True "Refinement successful" (Just $ JSON.String $ T.pack resultText)

mcpCaseSplit :: IORef ServerState -> Int -> String -> TCM AgdaResult
mcpCaseSplit stateRef goalIdParam varName = do
  state <- liftIO $ readIORef stateRef
  case currentFile state of
    Nothing -> pure $ AgdaResult False "No file loaded" Nothing
    Just _ -> do
      let iid = InteractionId goalIdParam
      (_, _, clauses) <- withInteractionId iid $ makeCase iid noRange varName
      -- Convert clauses to pretty text
      clauseDocs <- mapM prettyA clauses
      let clauseTexts = map (T.pack . render) clauseDocs
      pure $ AgdaResult True "Case split successful" (Just $ JSON.toJSON clauseTexts)

mcpCompute :: IORef ServerState -> Int -> String -> TCM AgdaResult
mcpCompute stateRef goalIdParam exprStr = do
  state <- liftIO $ readIORef stateRef
  case currentFile state of
    Nothing -> pure $ AgdaResult False "No file loaded" Nothing
    Just _ -> do
      let iid = InteractionId goalIdParam
      expr <- BasicOps.parseExprIn iid noRange exprStr
      -- For now, just return the parsed expression as we can't normalize Expr directly
      -- We would need to convert to internal syntax first
      doc <- prettyA expr
      let resultText = render doc
      pure $ AgdaResult True "Expression parsed" (Just $ JSON.String $ T.pack resultText)

mcpInferType :: IORef ServerState -> Int -> String -> TCM AgdaResult
mcpInferType stateRef goalIdParam exprStr = do
  state <- liftIO $ readIORef stateRef
  case currentFile state of
    Nothing -> pure $ AgdaResult False "No file loaded" Nothing
    Just _ -> do
      let iid = InteractionId goalIdParam
      expr <- BasicOps.parseExprIn iid noRange exprStr
      typeExpr <- withInteractionId iid $ BasicOps.typeInMeta iid AsIs expr
      resultDoc <- prettyTCM typeExpr
      let resultText = render resultDoc
      pure $ AgdaResult True "Type inferred" (Just $ JSON.String $ T.pack resultText)

mcpIntro :: IORef ServerState -> Int -> TCM AgdaResult
mcpIntro stateRef goalIdParam = do
  state <- liftIO $ readIORef stateRef
  case currentFile state of
    Nothing -> pure $ AgdaResult False "No file loaded" Nothing
    Just _ -> do
      let iid = InteractionId goalIdParam
      intros <- withInteractionId iid $ BasicOps.introTactic False iid
      pure $ AgdaResult True "Intro suggestions" (Just $ JSON.toJSON $ map T.pack intros)

mcpWhyInScope :: IORef ServerState -> String -> TCM AgdaResult
mcpWhyInScope stateRef name = do
  state <- liftIO $ readIORef stateRef
  case currentFile state of
    Nothing -> pure $ AgdaResult False "No file loaded" Nothing
    Just absPath -> do
      whyData <- BasicOps.whyInScope (show absPath) name
      let WhyInScopeData qname file _ defs mods = whyData
      let result = JSON.object
            [ "qualified_name" JSON..= T.pack (show qname)
            , "file" JSON..= T.pack file
            , "definitions" JSON..= (map (T.pack . show) defs)
            , "modules" JSON..= (map (T.pack . show) mods)
            ]
      pure $ AgdaResult True "Scope information" (Just result)

-- Helper functions

rangeToTuple :: Range -> (Int, Int, Int, Int)
rangeToTuple range =
  case (rStart range, rEnd range) of
    (Just startPos, Just endPos) ->
      (fromIntegral (posLine startPos), fromIntegral (posCol startPos),
       fromIntegral (posLine endPos), fromIntegral (posCol endPos))
    _ -> (0, 0, 0, 0)

renderConstraintType :: OutputConstraint Expr InteractionId -> TCM Text
renderConstraintType constraint = do
  -- Extract the type from the constraint and render it
  doc <- prettyA constraint
  return $ T.pack $ render doc

renderConstraintContext :: OutputConstraint Expr InteractionId -> TCM [AgdaContextEntry]
renderConstraintContext constraint = do
  -- For now, return empty context - we'll need to dig into the constraint structure
  -- to extract context entries. This would require pattern matching on the OutputConstraint
  -- which contains the context information.
  return []

-- ============================================================================
-- MCP Library Integration
-- ============================================================================

-- Initialize server state
initServerState :: IO (IORef ServerState)
initServerState = newIORef (ServerState Nothing Nothing)

-- MCP Tool Handler - wraps our existing Agda commands
-- This will be called by the mcp-server library when a tool is invoked
handleAgdaTool :: IORef ServerState -> AgdaMCP.Types.AgdaTool -> IO MCP.Server.Content
handleAgdaTool stateRef tool = do
  result <- runTCMTop $ case tool of
    AgdaMCP.Types.AgdaLoad{file} ->
      mcpLoadFile stateRef (T.unpack file)
    AgdaMCP.Types.AgdaGetGoals ->
      mcpGetGoals stateRef
    AgdaMCP.Types.AgdaGetGoalType{goalId} ->
      mcpGetGoalType stateRef goalId
    AgdaMCP.Types.AgdaGetContext{goalId} ->
      mcpGetContext stateRef goalId
    AgdaMCP.Types.AgdaGive{goalId, expression} ->
      mcpGive stateRef goalId (T.unpack expression)
    AgdaMCP.Types.AgdaRefine{goalId, expression} ->
      mcpRefine stateRef goalId (T.unpack expression)
    AgdaMCP.Types.AgdaCaseSplit{goalId, variable} ->
      mcpCaseSplit stateRef goalId (T.unpack variable)
    AgdaMCP.Types.AgdaCompute{goalId, expression} ->
      mcpCompute stateRef goalId (T.unpack expression)
    AgdaMCP.Types.AgdaInferType{goalId, expression} ->
      mcpInferType stateRef goalId (T.unpack expression)
    AgdaMCP.Types.AgdaIntro{goalId} ->
      mcpIntro stateRef goalId
    AgdaMCP.Types.AgdaWhyInScope{name} ->
      mcpWhyInScope stateRef (T.unpack name)

  case result of
    Left err ->
      -- Convert TCErr to MCP Content
      pure $ MCP.Server.ContentText $ "Error: " <> T.pack (show err)
    Right agdaRes ->
      -- Convert AgdaResult to MCP Content
      case agdaResult agdaRes of
        Nothing -> pure $ MCP.Server.ContentText $ message agdaRes
        Just val -> pure $ MCP.Server.ContentText $ TE.decodeUtf8 $ LBS.toStrict $ JSON.encode val

-- MCP Resource Handler - exposes Agda file information as resources
-- Resources extract parameters from the URI path
handleAgdaResource :: IORef ServerState -> MCP.Server.URI -> AgdaMCP.Types.AgdaResource -> IO MCP.Server.ResourceContent
handleAgdaResource stateRef uri resource = do
  result <- runTCMTop $ case resource of
    AgdaMCP.Types.Goals -> do
      -- URI format: resource://goals/{file}
      -- For now, just get goals from currently loaded file
      mcpGetGoals stateRef

    AgdaMCP.Types.GoalInfo -> do
      -- URI format: resource://goal_info/{file}/{id}
      -- For now, extract goal ID from URI path (simplified - would need proper URI parsing)
      -- Just return error for now as this requires URI path parsing
      pure $ AgdaResult False "GoalInfo resource not yet implemented - use agda_get_goal_type tool instead" Nothing

    AgdaMCP.Types.FileContext ->
      -- URI format: resource://file_context/{file}
      -- Return file context info
      pure $ AgdaResult True "File context" Nothing

  case result of
    Left err ->
      pure $ MCP.Server.ResourceText uri "text/plain" $ "Error: " <> T.pack (show err)
    Right agdaRes ->
      case agdaResult agdaRes of
        Nothing ->
          pure $ MCP.Server.ResourceText uri "text/plain" $ message agdaRes
        Just val ->
          pure $ MCP.Server.ResourceText uri "application/json" $ TE.decodeUtf8 $ LBS.toStrict $ JSON.encode val
