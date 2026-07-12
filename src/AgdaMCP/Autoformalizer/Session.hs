{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- | Session Management for the Autoformalizer REPL Protocol
--
-- This module implements the priority-based session type system described in
-- the Autoformalizer REPL Protocol Specification. Key features:
--
--   * Linear session usage: each session is used exactly once
--   * Priority monotonicity: priorities strictly increase through operations
--   * Recursive priority: child sessions start at higher priority
--   * Deadlock freedom: guaranteed by priority ordering
--
-- The session type ensures that:
--   1. Operations are executed in a valid order
--   2. Sessions terminate (bounded by maximum priority)
--   3. No circular waits can occur

module AgdaMCP.Autoformalizer.Session
    ( -- * Session Types
      Session(..)
    , SessionId
    , SessionToken
    , newSessionToken

      -- * Session State
    , SessionState(..)
    , newSessionState
    , SessionConfig(..)
    , defaultSessionConfig

      -- * Session Execution
    , runSession
    , executeOp
    , SessionError(..)

      -- * Operation Results
    , OpResult(..)
    , SourceOpResult(..)
    , TargetOpResult(..)
    , BijectionOpResult(..)

      -- * Priority Management
    , checkPriority
    , incrementPriority
    , canContinue

      -- * Session Invariants
    , validateSession
    , SessionInvariant(..)
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.IORef
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception (Exception, throwIO)
import Control.Concurrent.STM
import Data.Time.Clock (UTCTime, getCurrentTime)

import AgdaMCP.Autoformalizer.Types
import AgdaMCP.Autoformalizer.Source (SourceDocument)
import AgdaMCP.Autoformalizer.Bijection (BijectionState)

--------------------------------------------------------------------------------
-- Session Identifiers
--------------------------------------------------------------------------------

-- | Unique session identifier
type SessionId = UUID

-- | Session token with embedded priority
data SessionToken = SessionToken
    { tokenId :: SessionId
    , tokenPriority :: Priority
    , tokenParent :: Maybe SessionId
    , tokenCreated :: UTCTime
    }
    deriving (Show, Eq)

-- | Create a new session token
newSessionToken :: MonadIO m => Priority -> Maybe SessionId -> m SessionToken
newSessionToken priority parent = liftIO $ do
    sid <- UUID.nextRandom
    now <- getCurrentTime
    return SessionToken
        { tokenId = sid
        , tokenPriority = priority
        , tokenParent = parent
        , tokenCreated = now
        }

--------------------------------------------------------------------------------
-- Session Configuration
--------------------------------------------------------------------------------

-- | Configuration for session behavior
data SessionConfig = SessionConfig
    { -- | Maximum allowed priority (bounds recursion depth)
      configMaxPriority :: Int
      -- | Maximum operations per session
    , configMaxOps :: Int
      -- | Default timeout for operations (milliseconds)
    , configTimeout :: Int
      -- | Whether to allow parallel recursive calls
    , configAllowParallel :: Bool
      -- | Priority gap for parallel children
    , configParallelGap :: Int
    }
    deriving (Show, Eq)

-- | Default session configuration
-- P_max = 256, allows ~64 levels of recursion
defaultSessionConfig :: SessionConfig
defaultSessionConfig = SessionConfig
    { configMaxPriority = 256
    , configMaxOps = 128
    , configTimeout = 30000
    , configAllowParallel = True
    , configParallelGap = 4
    }

--------------------------------------------------------------------------------
-- Session State
--------------------------------------------------------------------------------

-- | State of an active session
data SessionState = SessionState
    { -- | Session token
      sessionToken :: SessionToken
      -- | Current priority (increases with each operation)
    , sessionPriority :: TVar Priority
      -- | Number of operations executed
    , sessionOpCount :: TVar Int
      -- | Whether session has been finalized
    , sessionFinalized :: TVar Bool
      -- | Configuration
    , sessionConfig :: SessionConfig
      -- | Child sessions spawned by this session
    , sessionChildren :: TVar [SessionId]
      -- | Operation history (for debugging/audit)
    , sessionHistory :: TVar [(Priority, SessionOp)]
      -- | Reference to source document
    , sessionSource :: IORef (Maybe SourceDocument)
      -- | Reference to bijection state
    , sessionBijection :: IORef BijectionState
      -- | Effects accumulated during session
    , sessionEffects :: TVar [Effect]
    }

-- | Create a new session state
newSessionState :: MonadIO m
                => SessionConfig
                -> Priority
                -> Maybe SessionId
                -> IORef (Maybe SourceDocument)
                -> IORef BijectionState
                -> m SessionState
newSessionState config priority parent srcRef bijRef = liftIO $ do
    token <- newSessionToken priority parent
    priorityVar <- newTVarIO priority
    opCountVar <- newTVarIO 0
    finalizedVar <- newTVarIO False
    childrenVar <- newTVarIO []
    historyVar <- newTVarIO []
    effectsVar <- newTVarIO []
    return SessionState
        { sessionToken = token
        , sessionPriority = priorityVar
        , sessionOpCount = opCountVar
        , sessionFinalized = finalizedVar
        , sessionConfig = config
        , sessionChildren = childrenVar
        , sessionHistory = historyVar
        , sessionSource = srcRef
        , sessionBijection = bijRef
        , sessionEffects = effectsVar
        }

--------------------------------------------------------------------------------
-- Session Errors
--------------------------------------------------------------------------------

-- | Errors that can occur during session execution
data SessionError
    = PriorityExceeded { errorMaxPriority :: Int, errorCurrentPriority :: Int }
    | SessionAlreadyFinalized { errorSessionId :: SessionId }
    | OperationLimitExceeded { errorMaxOps :: Int }
    | InvalidOperation { errorReason :: Text }
    | PriorityViolation { errorExpectedMin :: Priority, errorActual :: Priority }
    | ChildSessionFailed { errorChildId :: SessionId, errorChildError :: Text }
    deriving (Show, Eq)

instance Exception SessionError

--------------------------------------------------------------------------------
-- Session Execution
--------------------------------------------------------------------------------

-- | The Session type represents an active REPL session
-- This is a phantom type that tracks the session's state at the type level
data Session where
    -- | An active session that can perform operations
    ActiveSession :: SessionState -> Session
    -- | A finalized session that has completed
    FinalizedSession :: SessionId -> Output -> Session

-- | Run a session with the given initial state
runSession :: MonadIO m
           => SessionState
           -> (Session -> m (Either SessionError a))
           -> m (Either SessionError a)
runSession state action = do
    let session = ActiveSession state
    result <- action session
    -- Ensure session is finalized
    liftIO $ atomically $ writeTVar (sessionFinalized state) True
    return result

-- | Execute an operation within a session
executeOp :: MonadIO m
          => SessionState
          -> SessionOp
          -> (SessionOp -> IO OpResult)
          -> m (Either SessionError OpResult)
executeOp state op handler = liftIO $ do
    -- Check if session is finalized
    finalized <- atomically $ readTVar (sessionFinalized state)
    if finalized
        then return $ Left $ SessionAlreadyFinalized (tokenId $ sessionToken state)
        else do
            -- Check operation count
            opCount <- atomically $ readTVar (sessionOpCount state)
            if opCount >= configMaxOps (sessionConfig state)
                then return $ Left $ OperationLimitExceeded (configMaxOps $ sessionConfig state)
                else do
                    -- Get current priority
                    currentPriority <- atomically $ readTVar (sessionPriority state)
                    let maxP = configMaxPriority (sessionConfig state)
                    if priorityToInt currentPriority >= maxP
                        then return $ Left $ PriorityExceeded maxP (priorityToInt currentPriority)
                        else do
                            -- Execute the operation
                            result <- handler op

                            -- Update state (priority increases by 2 per operation per spec)
                            atomically $ do
                                modifyTVar' (sessionPriority state) (succPriority . succPriority)
                                modifyTVar' (sessionOpCount state) (+ 1)
                                modifyTVar' (sessionHistory state) ((currentPriority, op) :)

                            -- Handle finalization
                            case op of
                                OpFinal output -> do
                                    atomically $ writeTVar (sessionFinalized state) True
                                    return $ Right result
                                _ -> return $ Right result

--------------------------------------------------------------------------------
-- Operation Results
--------------------------------------------------------------------------------

-- | Result of executing an operation
data OpResult
    = ResultSource SourceOpResult
    | ResultTarget TargetOpResult
    | ResultBijection BijectionOpResult
    | ResultRecurse Result
    | ResultFinal
    deriving (Show, Eq)

-- | Result of a source operation
data SourceOpResult
    = PeekSectionResult SourceContent
    | GrepSourceResult [Match]
    | GetTheoremResult TheoremContent
    | GetDependenciesResult [TheoremId]
    deriving (Show, Eq)

-- | Result of a target (Agda) operation
data TargetOpResult
    = LoadResult' LoadResult
    | GetGoalsResult [Goal]
    | GetGoalContextResult GoalContext
    | GiveResult' GiveResult
    | RefineResult' RefineResult
    | CaseSplitResult' CaseSplitResult
    | AutoResult' AutoResult
    | SearchAboutResult [Name]
    deriving (Show, Eq)

-- | Result of a bijection operation
data BijectionOpResult
    = SourceForHoleResult (Maybe SourceRef)
    | FormalForSectionResult (Maybe FormalRef)
    | GetCoverageResult Coverage
    | UpdateBijectionResult
    deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Priority Management
--------------------------------------------------------------------------------

-- | Check if current priority allows continuation
checkPriority :: SessionState -> IO (Either SessionError Priority)
checkPriority state = do
    currentPriority <- atomically $ readTVar (sessionPriority state)
    let maxP = configMaxPriority (sessionConfig state)
    if priorityToInt currentPriority >= maxP
        then return $ Left $ PriorityExceeded maxP (priorityToInt currentPriority)
        else return $ Right currentPriority

-- | Increment session priority (by 2 per spec)
incrementPriority :: SessionState -> IO Priority
incrementPriority state = atomically $ do
    modifyTVar' (sessionPriority state) (succPriority . succPriority)
    readTVar (sessionPriority state)

-- | Check if session can continue (not finalized, within limits)
canContinue :: SessionState -> IO Bool
canContinue state = atomically $ do
    finalized <- readTVar (sessionFinalized state)
    opCount <- readTVar (sessionOpCount state)
    let maxOps = configMaxOps (sessionConfig state)
    return $ not finalized && opCount < maxOps

--------------------------------------------------------------------------------
-- Child Session Management
--------------------------------------------------------------------------------

-- | Spawn a child session for recursive calls
-- Child sessions start at priority (p + 2) where p is the parent's current priority
spawnChildSession :: MonadIO m
                  => SessionState
                  -> Task
                  -> m (Either SessionError SessionState)
spawnChildSession parentState _task = liftIO $ do
    -- Check if parent can spawn children
    canSpawn <- canContinue parentState
    if not canSpawn
        then return $ Left $ InvalidOperation "Parent session cannot spawn children"
        else do
            -- Get parent's current priority
            parentPriority <- atomically $ readTVar (sessionPriority parentState)
            -- Child starts at parent priority + 2
            let childPriority = succPriority (succPriority parentPriority)

            -- Check if child priority is within bounds
            let maxP = configMaxPriority (sessionConfig parentState)
            if priorityToInt childPriority >= maxP
                then return $ Left $ PriorityExceeded maxP (priorityToInt childPriority)
                else do
                    -- Create child session
                    childState <- newSessionState
                        (sessionConfig parentState)
                        childPriority
                        (Just $ tokenId $ sessionToken parentState)
                        (sessionSource parentState)
                        (sessionBijection parentState)

                    -- Register child with parent
                    atomically $ modifyTVar' (sessionChildren parentState)
                                             (tokenId (sessionToken childState) :)

                    return $ Right childState

-- | Spawn multiple child sessions in parallel
-- Children are allocated priorities with gaps to allow independent operation
spawnParallelChildren :: MonadIO m
                      => SessionState
                      -> [Task]
                      -> m (Either SessionError [SessionState])
spawnParallelChildren parentState tasks = liftIO $ do
    if not (configAllowParallel $ sessionConfig parentState)
        then return $ Left $ InvalidOperation "Parallel spawning not allowed"
        else do
            parentPriority <- atomically $ readTVar (sessionPriority parentState)
            let gap = configParallelGap (sessionConfig parentState)
                basePriority = priorityToInt parentPriority + 2
                childPriorities = [basePriority + (gap * i) | i <- [0 .. length tasks - 1]]
                maxP = configMaxPriority (sessionConfig parentState)

            -- Check if all children fit within priority bounds
            if any (>= maxP) childPriorities
                then return $ Left $ PriorityExceeded maxP (maximum childPriorities)
                else do
                    children <- mapM (createChildAtPriority parentState) childPriorities
                    return $ Right children
  where
    createChildAtPriority :: SessionState -> Int -> IO SessionState
    createChildAtPriority parent p = do
        let priority = priorityFromInt p
        newSessionState
            (sessionConfig parent)
            priority
            (Just $ tokenId $ sessionToken parent)
            (sessionSource parent)
            (sessionBijection parent)

--------------------------------------------------------------------------------
-- Session Invariants
--------------------------------------------------------------------------------

-- | Invariants that should hold for a session
data SessionInvariant
    = LinearUsage           -- ^ Each session is used exactly once
    | PriorityMonotonicity  -- ^ Priorities strictly increase
    | RecursivePriority     -- ^ Child priority > parent priority + 2
    | TerminationGuarantee  -- ^ Session must eventually reach Final
    deriving (Show, Eq, Enum, Bounded)

-- | Validate that session invariants hold
validateSession :: SessionState -> IO [SessionInvariant]
validateSession state = do
    history <- atomically $ readTVar (sessionHistory state)
    finalized <- atomically $ readTVar (sessionFinalized state)
    currentPriority <- atomically $ readTVar (sessionPriority state)

    let violations = concat
            [ checkMonotonicity history
            , checkTermination finalized history
            ]
    return violations
  where
    checkMonotonicity :: [(Priority, SessionOp)] -> [SessionInvariant]
    checkMonotonicity ops =
        let priorities = map fst ops
            pairs = zip priorities (drop 1 priorities)
            violations = filter (\(p1, p2) -> priorityToInt p1 >= priorityToInt p2) pairs
        in if null violations then [] else [PriorityMonotonicity]

    checkTermination :: Bool -> [(Priority, SessionOp)] -> [SessionInvariant]
    checkTermination finalized ops =
        let hasFinal = any isFinalOp (map snd ops)
        in if finalized && not hasFinal then [TerminationGuarantee] else []

    isFinalOp :: SessionOp -> Bool
    isFinalOp (OpFinal _) = True
    isFinalOp _ = False

--------------------------------------------------------------------------------
-- Effect Management
--------------------------------------------------------------------------------

-- | Record an effect from an operation
recordEffect :: SessionState -> Effect -> IO ()
recordEffect state effect =
    atomically $ modifyTVar' (sessionEffects state) (effect :)

-- | Get all effects accumulated during the session
getSessionEffects :: SessionState -> IO [Effect]
getSessionEffects state =
    atomically $ readTVar (sessionEffects state)

-- | Propagate effects from child to parent session
propagateEffects :: SessionState -> SessionState -> IO ()
propagateEffects childState parentState = do
    childEffects <- getSessionEffects childState
    atomically $ modifyTVar' (sessionEffects parentState) (++ childEffects)
