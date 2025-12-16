{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- | Handler Implementation for the Autoformalizer REPL Protocol
--
-- This module implements the environment-side handler interface from the
-- Autoformalizer REPL Protocol Specification. The handler:
--
--   * Executes Source operations against the source document
--   * Delegates Target operations to agda-mcp
--   * Manages Bijection state
--   * Spawns child sessions for recursive calls
--
-- The handler ensures all session invariants are maintained.

module AgdaMCP.Autoformalizer.Handler
    ( -- * Handler Type
      Handler(..)
    , newHandler
    , HandlerConfig(..)
    , defaultHandlerConfig

      -- * Handler Operations
    , handleOp
    , handleSourceOp
    , handleTargetOp
    , handleBijectionOp
    , handleRecurse
    , handleFinal

      -- * Handler State
    , HandlerState(..)

      -- * Agent Interface
    , Policy
    , runAgent
    , AgentContext(..)

      -- * Result Collection Strategies
    , CollectionStrategy(..)
    , collectResults
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.IORef
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent.Async (Async, async, waitAny, cancel, wait)
import Control.Concurrent.STM
import Control.Exception (try, SomeException)
import Data.Maybe (fromMaybe, catMaybes)

import AgdaMCP.Autoformalizer.Types
import AgdaMCP.Autoformalizer.Source
import AgdaMCP.Autoformalizer.Bijection
import AgdaMCP.Autoformalizer.Session

--------------------------------------------------------------------------------
-- Handler Configuration
--------------------------------------------------------------------------------

-- | Configuration for the handler
data HandlerConfig = HandlerConfig
    { -- | Session configuration
      handlerSessionConfig :: SessionConfig
      -- | Path to source document
    , handlerSourcePath :: Maybe FilePath
      -- | Path to bijection state file
    , handlerBijectionPath :: Maybe FilePath
      -- | Agda-MCP server URL (for target operations)
    , handlerAgdaMcpUrl :: Text
      -- | Default collection strategy for parallel results
    , handlerCollectionStrategy :: CollectionStrategy
    }
    deriving (Show, Eq)

-- | Default handler configuration
defaultHandlerConfig :: HandlerConfig
defaultHandlerConfig = HandlerConfig
    { handlerSessionConfig = defaultSessionConfig
    , handlerSourcePath = Nothing
    , handlerBijectionPath = Nothing
    , handlerAgdaMcpUrl = "http://localhost:3000/mcp"
    , handlerCollectionStrategy = CollectFirst
    }

--------------------------------------------------------------------------------
-- Handler State
--------------------------------------------------------------------------------

-- | State maintained by the handler
data HandlerState = HandlerState
    { -- | Source document
      handlerSource :: IORef (Maybe SourceDocument)
      -- | Bijection state
    , handlerBijection :: IORef BijectionState
      -- | Active sessions
    , handlerActiveSessions :: TVar [SessionState]
      -- | Handler configuration
    , handlerConfig :: HandlerConfig
    }

--------------------------------------------------------------------------------
-- Handler Type
--------------------------------------------------------------------------------

-- | The Handler executes operations from sessions
data Handler = Handler
    { -- | Handler state
      handlerState :: HandlerState
      -- | Execute a source operation
    , runSourceOp :: SourceOp -> IO SourceOpResult
      -- | Execute a target operation (delegates to agda-mcp)
    , runTargetOp :: TargetOp -> IO TargetOpResult
      -- | Execute a bijection operation
    , runBijectionOp :: BijectionOp -> IO BijectionOpResult
      -- | Spawn a child session for recursive call
    , runSpawnChild :: Priority -> Task -> IO Result
    }

-- | Create a new handler
newHandler :: MonadIO m => HandlerConfig -> m Handler
newHandler config = liftIO $ do
    -- Initialize source document
    sourceRef <- newIORef Nothing
    case handlerSourcePath config of
        Nothing -> return ()
        Just path -> do
            result <- loadSourceFromFile path
            case result of
                Left _err -> return ()
                Right doc -> writeIORef sourceRef (Just doc)

    -- Initialize bijection state
    bijRef <- newIORef emptyBijection
    case handlerBijectionPath config of
        Nothing -> return ()
        Just path -> do
            result <- loadBijection path
            case result of
                Left _err -> return ()
                Right bij -> writeIORef bijRef bij

    -- Initialize active sessions tracking
    activeSessionsVar <- newTVarIO []

    let state = HandlerState
            { handlerSource = sourceRef
            , handlerBijection = bijRef
            , handlerActiveSessions = activeSessionsVar
            , handlerConfig = config
            }

    return Handler
        { handlerState = state
        , runSourceOp = executeSourceOp state
        , runTargetOp = executeTargetOp state
        , runBijectionOp = executeBijectionOp state
        , runSpawnChild = executeSpawnChild state
        }

--------------------------------------------------------------------------------
-- Handler Operations
--------------------------------------------------------------------------------

-- | Handle any session operation
handleOp :: Handler -> SessionState -> SessionOp -> IO (Either SessionError OpResult)
handleOp handler session op = do
    executeOp session op $ \case
        OpSource srcOp -> ResultSource <$> runSourceOp handler srcOp
        OpTarget tgtOp -> ResultTarget <$> runTargetOp handler tgtOp
        OpBijection bijOp -> ResultBijection <$> runBijectionOp handler bijOp
        OpRecurse task -> do
            currentPriority <- atomically $ readTVar (sessionPriority session)
            ResultRecurse <$> runSpawnChild handler currentPriority task
        OpFinal output -> do
            -- Record session as finalized
            atomically $ writeTVar (sessionFinalized session) True
            return ResultFinal

-- | Handle a source operation
handleSourceOp :: Handler -> SourceOp -> IO SourceOpResult
handleSourceOp handler = runSourceOp handler

-- | Handle a target operation
handleTargetOp :: Handler -> TargetOp -> IO TargetOpResult
handleTargetOp handler = runTargetOp handler

-- | Handle a bijection operation
handleBijectionOp :: Handler -> BijectionOp -> IO BijectionOpResult
handleBijectionOp handler = runBijectionOp handler

-- | Handle a recursive call
handleRecurse :: Handler -> SessionState -> Task -> IO (Either SessionError Result)
handleRecurse handler session task = do
    currentPriority <- atomically $ readTVar (sessionPriority session)
    result <- runSpawnChild handler currentPriority task
    return $ Right result

-- | Handle session finalization
handleFinal :: Handler -> SessionState -> Output -> IO ()
handleFinal _handler session _output = do
    atomically $ writeTVar (sessionFinalized session) True

--------------------------------------------------------------------------------
-- Source Operation Execution
--------------------------------------------------------------------------------

-- | Execute a source operation
executeSourceOp :: HandlerState -> SourceOp -> IO SourceOpResult
executeSourceOp state op = do
    mDoc <- readIORef (handlerSource state)
    case mDoc of
        Nothing -> return $ case op of
            PeekSection _ -> PeekSectionResult $ SourceContent "" []
            GrepSource _ -> GrepSourceResult []
            GetTheorem _ -> GetTheoremResult $ TheoremContent "" Nothing
            GetDependencies _ -> GetDependenciesResult []
        Just doc -> case op of
            PeekSection sid ->
                case peekSection doc sid of
                    Left _err -> return $ PeekSectionResult $ SourceContent "" []
                    Right content -> return $ PeekSectionResult content

            GrepSource pat ->
                return $ GrepSourceResult $ grepSource doc pat

            GetTheorem tid ->
                case getTheorem doc tid of
                    Left _err -> return $ GetTheoremResult $ TheoremContent "" Nothing
                    Right content -> return $ GetTheoremResult content

            GetDependencies tid ->
                case getDependencies doc tid of
                    Left _err -> return $ GetDependenciesResult []
                    Right deps -> return $ GetDependenciesResult deps

--------------------------------------------------------------------------------
-- Target Operation Execution (delegates to agda-mcp)
--------------------------------------------------------------------------------

-- | Execute a target operation by delegating to agda-mcp
-- This is a stub that would need to make actual HTTP calls to the agda-mcp server
executeTargetOp :: HandlerState -> TargetOp -> IO TargetOpResult
executeTargetOp state op = case op of
    AgdaLoadOp modPath ->
        -- Would call AgdaLoad via HTTP
        -- For now, return a placeholder
        return $ LoadResult' $ LoadError "Not connected to agda-mcp"

    AgdaGetGoalsOp ->
        -- Would call AgdaGetGoals via HTTP
        return $ GetGoalsResult []

    AgdaGetGoalContextOp hid ->
        -- Would call AgdaGetGoalContext via HTTP
        return $ GetGoalContextResult $ GoalContext
            { gcGoal = Goal hid "" []
            , gcLocalNames = []
            , gcAvailableLemmas = []
            }

    AgdaGiveOp hid term ->
        -- Would call AgdaGive via HTTP
        return $ GiveResult' $ GiveError "Not connected to agda-mcp"

    AgdaRefineOp hid term ->
        -- Would call AgdaRefine via HTTP
        return $ RefineResult' $ RefineError "Not connected to agda-mcp"

    AgdaCaseSplitOp hid var ->
        -- Would call AgdaCaseSplit via HTTP
        return $ CaseSplitResult' $ SplitError "Not connected to agda-mcp"

    AgdaAutoOp hid ->
        -- Would call AgdaAuto via HTTP
        return $ AutoResult' AutoFailed

    AgdaSearchAboutOp query ->
        -- Would call AgdaSearchAbout via HTTP
        return $ SearchAboutResult []

--------------------------------------------------------------------------------
-- Bijection Operation Execution
--------------------------------------------------------------------------------

-- | Execute a bijection operation
executeBijectionOp :: HandlerState -> BijectionOp -> IO BijectionOpResult
executeBijectionOp state op = do
    bij <- readIORef (handlerBijection state)
    case op of
        SourceForHole hid ->
            return $ SourceForHoleResult $ sourceForHole bij hid

        FormalForSection sid ->
            return $ FormalForSectionResult $ formalForSection bij sid

        GetCoverage ->
            return $ GetCoverageResult $ getCoverage bij

        UpdateBijection srcRef formalRef -> do
            let newBij = updateBijection bij srcRef formalRef
            writeIORef (handlerBijection state) newBij
            return UpdateBijectionResult

--------------------------------------------------------------------------------
-- Recursive Call Execution
--------------------------------------------------------------------------------

-- | Execute a recursive call by spawning a child session
executeSpawnChild :: HandlerState -> Priority -> Task -> IO Result
executeSpawnChild state priority task = do
    -- Create child session at priority + 2
    let childPriority = succPriority (succPriority priority)

    childSession <- newSessionState
        (handlerSessionConfig $ handlerConfig state)
        childPriority
        Nothing  -- No parent tracking needed for this
        (handlerSource state)
        (handlerBijection state)

    -- Register child session
    atomically $ modifyTVar' (handlerActiveSessions state) (childSession :)

    -- Execute child session (this would normally involve agent interaction)
    -- For now, return a placeholder result
    let result = Result
            { resultStatus = Partial "Child session placeholder"
            , resultOutput = Nothing
            , resultEffects = []
            }

    -- Unregister child session
    atomically $ modifyTVar' (handlerActiveSessions state)
                             (filter (\s -> tokenId (sessionToken s) /= tokenId (sessionToken childSession)))

    return result

--------------------------------------------------------------------------------
-- Agent Interface
--------------------------------------------------------------------------------

-- | Agent context provided to the policy
data AgentContext = AgentContext
    { -- | Current task
      agentTask :: Task
      -- | Current session state
    , agentSession :: SessionState
      -- | Handler for executing operations
    , agentHandler :: Handler
    }

-- | Policy type: given context, selects an operation
type Policy = AgentContext -> IO SessionOp

-- | Run an agent (LLM) with a policy
runAgent :: Handler -> Task -> Policy -> IO Result
runAgent handler task policy = do
    -- Create session for this task
    session <- newSessionState
        (handlerSessionConfig $ handlerConfig $ handlerState handler)
        PriorityZ  -- Start at base priority
        Nothing
        (handlerSource $ handlerState handler)
        (handlerBijection $ handlerState handler)

    -- Agent loop
    runAgentLoop handler session task policy

-- | Main agent execution loop
runAgentLoop :: Handler -> SessionState -> Task -> Policy -> IO Result
runAgentLoop handler session task policy = do
    let context = AgentContext
            { agentTask = task
            , agentSession = session
            , agentHandler = handler
            }

    -- Get next operation from policy
    op <- policy context

    -- Execute operation
    result <- handleOp handler session op

    case result of
        Left err ->
            -- Session error, return failure
            return Result
                { resultStatus = Failed $ T.pack $ show err
                , resultOutput = Nothing
                , resultEffects = []
                }

        Right ResultFinal ->
            -- Session complete
            do
                effects <- atomically $ readTVar (sessionEffects session)
                return Result
                    { resultStatus = Success
                    , resultOutput = Nothing
                    , resultEffects = effects
                    }

        Right _ ->
            -- Continue execution
            runAgentLoop handler session task policy

--------------------------------------------------------------------------------
-- Result Collection Strategies
--------------------------------------------------------------------------------

-- | Strategy for collecting results from parallel recursive calls
data CollectionStrategy
    = CollectFirst   -- ^ Return first successful result, cancel others
    | CollectAll     -- ^ Wait for all, combine results
    | CollectBest    -- ^ Wait for all, select best by ranking
    deriving (Show, Eq)

-- | Collect results from parallel tasks
collectResults :: CollectionStrategy
               -> Handler
               -> [(Task, Policy)]
               -> IO [Result]
collectResults strategy handler taskPolicies = case strategy of
    CollectFirst -> collectFirst handler taskPolicies
    CollectAll -> collectAll handler taskPolicies
    CollectBest -> collectAll handler taskPolicies  -- Same as collectAll for now

-- | Collect first successful result
collectFirst :: Handler -> [(Task, Policy)] -> IO [Result]
collectFirst handler taskPolicies = do
    -- Start all tasks in parallel
    asyncs <- mapM (\(task, policy) -> async $ runAgent handler task policy) taskPolicies

    -- Wait for first success
    let waitForSuccess :: [Async Result] -> IO (Maybe Result)
        waitForSuccess [] = return Nothing
        waitForSuccess as = do
            (completed, result) <- waitAny as
            case resultStatus result of
                Success -> do
                    -- Cancel remaining
                    mapM_ cancel (filter (/= completed) as)
                    return $ Just result
                _ ->
                    -- Try next
                    waitForSuccess (filter (/= completed) as)

    mResult <- waitForSuccess asyncs
    return $ maybe [] (: []) mResult

-- | Collect all results
collectAll :: Handler -> [(Task, Policy)] -> IO [Result]
collectAll handler taskPolicies = do
    -- Start all tasks in parallel
    asyncs <- mapM (\(task, policy) -> async $ runAgent handler task policy) taskPolicies

    -- Wait for all
    mapM wait asyncs
