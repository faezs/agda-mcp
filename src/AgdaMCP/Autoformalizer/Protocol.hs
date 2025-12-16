{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- | Autoformalizer REPL Protocol
--
-- This module provides the top-level API for the Autoformalizer REPL Protocol.
-- The protocol enables session-typed interaction with an autoformalizer that
-- completes partial Agda formalizations guided by source documents.
--
-- = Overview
--
-- The system has three components:
--
--   * __Source__: A document being formalized (read-only)
--   * __Target__: An Agda project with holes and postulates (read-write via agda-mcp)
--   * __Bijection__: Correspondence between source elements and formal elements
--
-- = Protocol Properties
--
-- The protocol guarantees:
--
--   * __Deadlock Freedom__: No execution can deadlock (by priority ordering)
--   * __Session Fidelity__: Every session is used according to its type
--   * __Termination__: Given finite priority bound, every session terminates
--
-- = Usage
--
-- @
-- -- Create a handler
-- handler <- newHandler defaultHandlerConfig
--     { handlerSourcePath = Just "paper.tex"
--     , handlerBijectionPath = Just "bijection.json"
--     }
--
-- -- Define a task
-- let task = Task
--         { taskFocus = FillHole (HoleId 0)
--         , taskSourceSlice = Nothing
--         , taskTargetModule = Just (ModulePath "Neural.Homotopy.GammaSpaces")
--         , taskRelevantBijection = []
--         }
--
-- -- Run with a policy
-- result <- runProtocol handler task myPolicy
-- @

module AgdaMCP.Autoformalizer.Protocol
    ( -- * Protocol Entry Points
      runProtocol
    , runProtocolParallel

      -- * Protocol Configuration
    , ProtocolConfig(..)
    , defaultProtocolConfig

      -- * Re-exports
    , module AgdaMCP.Autoformalizer.Types
    , module AgdaMCP.Autoformalizer.Handler
    , module AgdaMCP.Autoformalizer.Session
    , module AgdaMCP.Autoformalizer.Source
    , module AgdaMCP.Autoformalizer.Bijection

      -- * Example Policies
    , simplePolicy
    , autoFillPolicy
    , explorativePolicy

      -- * Protocol Traces
    , Trace(..)
    , TraceEntry(..)
    , recordTrace
    , replayTrace
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.IORef
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent.STM
import Data.Time.Clock (UTCTime, getCurrentTime)

import AgdaMCP.Autoformalizer.Types
import AgdaMCP.Autoformalizer.Source
import AgdaMCP.Autoformalizer.Bijection
import AgdaMCP.Autoformalizer.Session
import AgdaMCP.Autoformalizer.Handler

--------------------------------------------------------------------------------
-- Protocol Configuration
--------------------------------------------------------------------------------

-- | Configuration for protocol execution
data ProtocolConfig = ProtocolConfig
    { -- | Handler configuration
      protocolHandlerConfig :: HandlerConfig
      -- | Maximum recursion depth
    , protocolMaxRecursion :: Int
      -- | Whether to record execution trace
    , protocolRecordTrace :: Bool
      -- | Timeout for entire protocol execution (ms)
    , protocolTimeout :: Maybe Int
    }
    deriving (Show, Eq)

-- | Default protocol configuration
defaultProtocolConfig :: ProtocolConfig
defaultProtocolConfig = ProtocolConfig
    { protocolHandlerConfig = defaultHandlerConfig
    , protocolMaxRecursion = 64
    , protocolRecordTrace = False
    , protocolTimeout = Nothing
    }

--------------------------------------------------------------------------------
-- Protocol Entry Points
--------------------------------------------------------------------------------

-- | Run the autoformalizer protocol with a single task
runProtocol :: Handler -> Task -> Policy -> IO Result
runProtocol = runAgent

-- | Run the autoformalizer protocol with parallel tasks
runProtocolParallel :: Handler -> [(Task, Policy)] -> CollectionStrategy -> IO [Result]
runProtocolParallel = collectResults

--------------------------------------------------------------------------------
-- Example Policies
--------------------------------------------------------------------------------

-- | Simple policy that tries auto first, then gives up
simplePolicy :: Policy
simplePolicy ctx = do
    case taskFocus (agentTask ctx) of
        FillHole hid -> do
            -- First try auto
            return $ OpTarget $ AgdaAutoOp hid

        FormalizeSection sid ->
            -- Get section content and finalize
            return $ OpSource $ PeekSection sid

        ProvePostulate name ->
            -- Try to find source for postulate
            return $ OpFinal $ Output Nothing [] (Partial "Postulate proving not implemented")

-- | Policy that attempts automatic hole filling
autoFillPolicy :: Policy
autoFillPolicy ctx = do
    let session = agentSession ctx
    opCount <- atomically $ readTVar (sessionOpCount session)

    case taskFocus (agentTask ctx) of
        FillHole hid -> do
            if opCount == 0
                then return $ OpTarget $ AgdaAutoOp hid
                else return $ OpFinal $ Output Nothing [] (Partial "Auto failed")

        _ ->
            return $ OpFinal $ Output Nothing [] (Partial "Not a hole filling task")

-- | Explorative policy that gathers context before attempting
explorativePolicy :: Policy
explorativePolicy ctx = do
    let session = agentSession ctx
        task = agentTask ctx
    opCount <- atomically $ readTVar (sessionOpCount session)

    case taskFocus task of
        FillHole hid -> exploratoryHoleFill ctx hid opCount

        FormalizeSection sid -> exploratoryFormalize ctx sid opCount

        ProvePostulate name -> do
            return $ OpFinal $ Output Nothing [] (Partial "Postulate proving not implemented")

-- | Explorative hole filling strategy
exploratoryHoleFill :: AgentContext -> HoleId -> Int -> IO SessionOp
exploratoryHoleFill ctx hid opCount
    | opCount == 0 = do
        -- Step 1: Get goal context
        return $ OpTarget $ AgdaGetGoalContextOp hid

    | opCount == 1 = do
        -- Step 2: Check if there's a source reference
        return $ OpBijection $ SourceForHole hid

    | opCount == 2 = do
        -- Step 3: Try auto
        return $ OpTarget $ AgdaAutoOp hid

    | otherwise = do
        -- Give up
        return $ OpFinal $ Output Nothing [] (Partial "Exploration exhausted")

-- | Explorative section formalization strategy
exploratoryFormalize :: AgentContext -> SectionId -> Int -> IO SessionOp
exploratoryFormalize ctx sid opCount
    | opCount == 0 = do
        -- Step 1: Get section content
        return $ OpSource $ PeekSection sid

    | opCount == 1 = do
        -- Step 2: Check if already formalized
        return $ OpBijection $ FormalForSection sid

    | opCount == 2 = do
        -- Step 3: Get coverage
        return $ OpBijection GetCoverage

    | otherwise = do
        -- Done exploring
        return $ OpFinal $ Output Nothing [] (Partial "Section exploration complete")

--------------------------------------------------------------------------------
-- Protocol Traces
--------------------------------------------------------------------------------

-- | An entry in the execution trace
data TraceEntry = TraceEntry
    { traceTimestamp :: UTCTime
    , tracePriority :: Priority
    , traceOp :: SessionOp
    , traceResult :: OpResult
    }
    deriving (Show, Eq)

-- | A complete execution trace
data Trace = Trace
    { traceEntries :: [TraceEntry]
    , traceStartTime :: UTCTime
    , traceEndTime :: Maybe UTCTime
    , traceSessionId :: SessionId
    , traceFinalResult :: Maybe Result
    }
    deriving (Show, Eq)

-- | Record an operation to the trace
recordTrace :: TVar Trace -> Priority -> SessionOp -> OpResult -> IO ()
recordTrace traceVar priority op result = do
    now <- getCurrentTime
    let entry = TraceEntry
            { traceTimestamp = now
            , tracePriority = priority
            , traceOp = op
            , traceResult = result
            }
    atomically $ modifyTVar' traceVar $ \trace ->
        trace { traceEntries = traceEntries trace ++ [entry] }

-- | Replay a trace (useful for debugging/testing)
replayTrace :: Handler -> Trace -> IO [OpResult]
replayTrace handler trace = do
    session <- newSessionState
        (handlerSessionConfig $ handlerConfig $ handlerState handler)
        PriorityZ
        Nothing
        (handlerSource $ handlerState handler)
        (handlerBijection $ handlerState handler)

    mapM (replayEntry handler session) (traceEntries trace)

-- | Replay a single trace entry
replayEntry :: Handler -> SessionState -> TraceEntry -> IO OpResult
replayEntry handler session entry = do
    result <- handleOp handler session (traceOp entry)
    case result of
        Left err -> return ResultFinal  -- Error during replay
        Right res -> return res

--------------------------------------------------------------------------------
-- Protocol Utilities
--------------------------------------------------------------------------------

-- | Create a handler and run a task in one call
runOneShot :: ProtocolConfig -> Task -> Policy -> IO Result
runOneShot config task policy = do
    handler <- newHandler (protocolHandlerConfig config)
    runProtocol handler task policy

-- | Check if a result is successful
isSuccess :: Result -> Bool
isSuccess result = case resultStatus result of
    Success -> True
    _ -> False

-- | Check if a result is partial
isPartial :: Result -> Bool
isPartial result = case resultStatus result of
    Partial _ -> True
    _ -> False

-- | Check if a result failed
isFailed :: Result -> Bool
isFailed result = case resultStatus result of
    Failed _ -> True
    _ -> False

-- | Extract the reason from a non-success result
getReason :: Result -> Maybe Text
getReason result = case resultStatus result of
    Partial r -> Just r
    Failed r -> Just r
    Success -> Nothing

-- | Combine multiple results
combineResults :: [Result] -> Result
combineResults [] = Result
    { resultStatus = Failed "No results"
    , resultOutput = Nothing
    , resultEffects = []
    }
combineResults [r] = r
combineResults results =
    let effects = concatMap resultEffects results
        outputs = [t | Result _ (Just t) _ <- results]
        successes = filter isSuccess results
        partials = filter isPartial results
    in if not (null successes)
       then (head successes) { resultEffects = effects }
       else if not (null partials)
            then (head partials) { resultEffects = effects }
            else Result
                { resultStatus = Failed "All attempts failed"
                , resultOutput = if null outputs then Nothing else Just (head outputs)
                , resultEffects = effects
                }
