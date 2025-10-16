{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : AgdaMCP.SessionManager
Description : Session management for multi-agent Agda REPL isolation
Copyright   : (c) 2025
License     : MIT

This module provides session management for the Agda MCP server, allowing
multiple agents to work with separate REPL instances without interference.

Key features:
- Lazy REPL initialization: REPLs created on first use
- Session isolation: each session ID gets its own ServerState and REPL
- Automatic garbage collection: idle sessions cleaned up after timeout
- Backward compatibility: Nothing = shared default session
-}

module AgdaMCP.SessionManager
  ( -- * Types
    SessionManager
  , SessionId
  , SessionMetadata(..)

  -- * Creation and Destruction
  , createSessionManager
  , destroySessionManager

  -- * Session Operations
  , getOrCreateSession
  , updateLastUsed
  , getSessionStats

  -- * Configuration
  , SessionConfig(..)
  , defaultSessionConfig
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forM_, when)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import System.IO (hPutStrLn, stderr)

-- ============================================================================
-- Types
-- ============================================================================

-- | Session identifier (UUID as Text for JSON compatibility)
type SessionId = Text

-- | Metadata about an active session
-- We use a polymorphic type parameter 'state' instead of importing Server.ServerState
-- This breaks the circular dependency
data SessionMetadata state = SessionMetadata
  { sessionId :: SessionId
  , sessionState :: IORef state
  , sessionCreatedAt :: UTCTime
  , sessionLastUsed :: TVar UTCTime
  }

-- | Session manager configuration
data SessionConfig = SessionConfig
  { sessionTimeout :: Int  -- ^ Idle timeout in seconds (default: 600 = 10 minutes)
  , gcInterval :: Int      -- ^ GC check interval in seconds (default: 60)
  , maxSessions :: Maybe Int  -- ^ Optional max sessions limit (Nothing = unlimited)
  } deriving (Show, Eq)

-- | Default configuration
defaultSessionConfig :: SessionConfig
defaultSessionConfig = SessionConfig
  { sessionTimeout = 600   -- 10 minutes
  , gcInterval = 60        -- 1 minute
  , maxSessions = Nothing  -- Unlimited
  }

-- | Session manager state (polymorphic over state type)
data SessionManager state = SessionManager
  { -- Map from session ID to session metadata
    sessions :: TVar (Map SessionId (SessionMetadata state))

    -- Default session (backward compatibility for sessionId = Nothing)
  , defaultSession :: SessionMetadata state

    -- Configuration
  , config :: SessionConfig

    -- GC thread ID
  , gcThreadId :: ThreadId

    -- Function to create new state
  , stateInitializer :: IO (IORef state)

    -- Optional cleanup function for state
  , stateCleanup :: Maybe (IORef state -> IO ())
  }

-- ============================================================================
-- Creation and Destruction
-- ============================================================================

-- | Create a new session manager with default configuration
-- Takes a state initializer function (like Server.initServerState)
-- and an optional cleanup function
createSessionManager :: IO (IORef state) -> Maybe (IORef state -> IO ()) -> IO (SessionManager state)
createSessionManager initializer cleanup =
  createSessionManagerWithConfig defaultSessionConfig initializer cleanup

-- | Create a new session manager with custom configuration
createSessionManagerWithConfig :: SessionConfig
                                -> IO (IORef state)
                                -> Maybe (IORef state -> IO ())
                                -> IO (SessionManager state)
createSessionManagerWithConfig cfg initializer cleanup = do
  -- Create the default session (for backward compatibility)
  now <- getCurrentTime
  defaultState <- initializer
  lastUsed <- newTVarIO now
  let defaultMeta = SessionMetadata
        { sessionId = "default"
        , sessionState = defaultState
        , sessionCreatedAt = now
        , sessionLastUsed = lastUsed
        }

  -- Create session map
  sessionsVar <- newTVarIO Map.empty

  -- Create the manager (without GC thread yet)
  let mgr = SessionManager
        { sessions = sessionsVar
        , defaultSession = defaultMeta
        , config = cfg
        , gcThreadId = error "GC thread not started yet"
        , stateInitializer = initializer
        , stateCleanup = cleanup
        }

  -- Start garbage collection thread
  gcThread <- forkIO $ gcLoop mgr

  -- Return manager with GC thread
  let finalMgr = mgr { gcThreadId = gcThread }

  hPutStrLn stderr $ "SessionManager: Created with " ++ show cfg
  return finalMgr

-- | Destroy session manager and clean up all sessions
destroySessionManager :: SessionManager state -> IO ()
destroySessionManager mgr = do
  -- Kill GC thread
  killThread (gcThreadId mgr)

  -- Get all sessions
  allSessions <- atomically $ do
    sessMap <- readTVar (sessions mgr)
    return $ Map.elems sessMap

  -- Clean up all sessions using the cleanup function
  case stateCleanup mgr of
    Just cleanup -> forM_ allSessions $ \meta -> do
      hPutStrLn stderr $ "SessionManager: Cleaning up session " ++ T.unpack (sessionId meta)
      cleanup (sessionState meta)
    Nothing -> return ()

  hPutStrLn stderr "SessionManager: Destroyed"

-- ============================================================================
-- Session Operations
-- ============================================================================

-- | Get existing session or create new one
-- If sessionIdMaybe is Nothing, returns the default shared session
getOrCreateSession :: SessionManager state -> Maybe Text -> IO (IORef state)
getOrCreateSession mgr Nothing = do
  -- Use default session for backward compatibility
  now <- getCurrentTime
  atomically $ writeTVar (sessionLastUsed (defaultSession mgr)) now
  return $ sessionState (defaultSession mgr)

getOrCreateSession mgr (Just sid) = do
  -- Check if session exists
  maybeMeta <- atomically $ do
    sessMap <- readTVar (sessions mgr)
    return $ Map.lookup sid sessMap

  case maybeMeta of
    Just meta -> do
      -- Session exists, update last used and return
      now <- getCurrentTime
      atomically $ writeTVar (sessionLastUsed meta) now
      hPutStrLn stderr $ "SessionManager: Reusing session " ++ T.unpack sid
      return $ sessionState meta

    Nothing -> do
      -- Create new session
      hPutStrLn stderr $ "SessionManager: Creating new session " ++ T.unpack sid

      -- Check session limit
      sessMap <- readTVarIO (sessions mgr)
      case maxSessions (config mgr) of
        Just limit | Map.size sessMap >= limit -> do
          error $ "Session limit reached (" ++ show limit ++ " sessions)"
        _ -> return ()

      -- Create new state using initializer
      now <- getCurrentTime
      newState <- stateInitializer mgr
      lastUsed <- newTVarIO now
      let meta = SessionMetadata
            { sessionId = sid
            , sessionState = newState
            , sessionCreatedAt = now
            , sessionLastUsed = lastUsed
            }

      -- Register session
      atomically $ modifyTVar' (sessions mgr) $ Map.insert sid meta

      hPutStrLn stderr $ "SessionManager: Session " ++ T.unpack sid ++ " created successfully"
      return $ sessionState meta

-- | Update last used timestamp for a session
updateLastUsed :: SessionManager state -> Maybe Text -> IO ()
updateLastUsed mgr Nothing = do
  -- Update default session
  now <- getCurrentTime
  atomically $ writeTVar (sessionLastUsed (defaultSession mgr)) now

updateLastUsed mgr (Just sid) = do
  maybeMeta <- atomically $ do
    sessMap <- readTVar (sessions mgr)
    return $ Map.lookup sid sessMap

  case maybeMeta of
    Just meta -> do
      now <- getCurrentTime
      atomically $ writeTVar (sessionLastUsed meta) now
    Nothing -> return ()  -- Session doesn't exist, ignore

-- | Get statistics about active sessions
getSessionStats :: SessionManager state -> IO (Int, [SessionId])
getSessionStats mgr = do
  sessMap <- readTVarIO (sessions mgr)
  let count = Map.size sessMap
  let ids = Map.keys sessMap
  return (count, ids)

-- ============================================================================
-- Garbage Collection
-- ============================================================================

-- | Background garbage collection loop
-- Checks for idle sessions and cleans them up
gcLoop :: SessionManager state -> IO ()
gcLoop mgr = do
  let intervalMicros = gcInterval (config mgr) * 1000000
  let timeoutSeconds = fromIntegral $ sessionTimeout (config mgr)

  -- Sleep before first check
  threadDelay intervalMicros

  -- Run GC cycle
  now <- getCurrentTime
  toKill <- atomically $ do
    sessMap <- readTVar (sessions mgr)

    -- Partition into keep/kill based on idle time
    let checkIdle meta = do
          lastUsed <- readTVar (sessionLastUsed meta)
          let idle = diffUTCTime now lastUsed
          return (idle < timeoutSeconds, meta)

    results <- mapM checkIdle (Map.elems sessMap)
    let (keep, kill) = partitionEithers [(if ok then Left m else Right m, m) | (ok, m) <- results]

    -- Update map to keep only active sessions
    let keepMap = Map.fromList [(sessionId m, m) | (_, m) <- keep]
    writeTVar (sessions mgr) keepMap

    return $ map snd kill

  -- Clean up timed-out sessions using cleanup function
  when (not $ null toKill) $ do
    hPutStrLn stderr $ "SessionManager GC: Cleaning up " ++ show (length toKill) ++ " idle sessions"
    case stateCleanup mgr of
      Just cleanup -> forM_ toKill $ \meta -> do
        hPutStrLn stderr $ "SessionManager GC: Cleaning session " ++ T.unpack (sessionId meta)
        cleanup (sessionState meta)
      Nothing -> return ()

  -- Continue loop
  gcLoop mgr
  where
    -- Helper to partition with Either
    partitionEithers :: [(Either a b, c)] -> ([(a, c)], [(b, c)])
    partitionEithers [] = ([], [])
    partitionEithers ((Left a, c):xs) =
      let (ls, rs) = partitionEithers xs
      in ((a, c):ls, rs)
    partitionEithers ((Right b, c):xs) =
      let (ls, rs) = partitionEithers xs
      in (ls, (b, c):rs)
