{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, toJSON)
import qualified Data.Aeson as JSON
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import System.IO (hSetEncoding, stderr, stdout, utf8, hPutStrLn)
import System.IO.Unsafe (unsafePerformIO)

import MCP.Protocol hiding (error)
import MCP.Server hiding (ServerState)
import MCP.Server.HTTP
import MCP.Types

import AgdaMCP.Server
import AgdaMCP.Types
import qualified AgdaMCP.SessionManager as SessionManager

-- | Use IORef for global session manager
{-# NOINLINE globalSessionManagerRef #-}
globalSessionManagerRef :: IORef (Maybe (SessionManager.SessionManager ServerState))
globalSessionManagerRef = unsafePerformIO $ newIORef Nothing

setGlobalSessionManager :: SessionManager.SessionManager ServerState -> IO ()
setGlobalSessionManager mgr = writeIORef globalSessionManagerRef (Just mgr)

getGlobalSessionManager :: IO (SessionManager.SessionManager ServerState)
getGlobalSessionManager = do
    mMgr <- readIORef globalSessionManagerRef
    case mMgr of
        Just mgr -> return mgr
        Nothing -> error "Session manager not initialized"

-- | Instance of MCPServer for our Agda MCP Server
-- We need to use MCPServerM monad for the implementation
instance MCPServer MCPServerM where
    handleListTools _params = do
        let tools = agdaTools
        return $ ListToolsResult
            { tools = tools
            , nextCursor = Nothing
            , _meta = Nothing
            }

    handleCallTool CallToolParams{name = toolName, arguments = mArgs} = do
        result <- liftIO $ do
            -- Get session manager from global state
            manager <- getGlobalSessionManager
            let args = fromMaybe Map.empty mArgs
            callAgdaTool manager toolName args
        return result

    -- Resources - use default implementations for now
    handleListResources _params = do
        return $ ListResourcesResult
            { resources = agdaResources
            , nextCursor = Nothing
            , _meta = Nothing
            }

    handleReadResource ReadResourceParams{uri = resourceUri} = do
        result <- liftIO $ do
            manager <- getGlobalSessionManager
            readAgdaResource manager resourceUri
        return result

    -- Prompts - not implemented yet
    handleListPrompts _params = return $ ListPromptsResult
        { prompts = []
        , nextCursor = Nothing
        , _meta = Nothing
        }

    handleGetPrompt _params = return $ GetPromptResult
        { messages = []
        , description = Nothing
        , _meta = Nothing
        }

    -- Other handlers use defaults
    handleSetLevel _params = liftIO $ putStrLn "Log level set"

-- | Define the Agda tools list for the MCP protocol
agdaTools :: [Tool]
agdaTools =
    [ mkTool "agda_load" "Load and type-check an Agda file"
        [("file", "string", "Path to the Agda file", True)
        ,("sessionId", "string", "Optional session ID for multi-agent isolation", False)
        ,("format", "string", "Response format: Concise (default) or Full", False)
        ]
    , mkTool "agda_get_goals" "List all goals/holes in the currently loaded file"
        [("sessionId", "string", "Optional session ID for multi-agent isolation", False)
        ,("format", "string", "Response format: Concise (default) or Full", False)
        ]
    , mkTool "agda_get_goal_type" "Get the type of a specific goal"
        [("goalId", "integer", "The numeric ID of the goal/hole (starting from 0)", True)
        ,("sessionId", "string", "Optional session ID for multi-agent isolation", False)
        ,("format", "string", "Response format: Concise (default) or Full", False)
        ]
    , mkTool "agda_get_goal_type_implicits" "Get the type of a specific goal with implicit arguments shown"
        [("goalId", "integer", "The numeric ID of the goal/hole (starting from 0)", True)
        ,("sessionId", "string", "Optional session ID for multi-agent isolation", False)
        ,("format", "string", "Response format: Concise (default) or Full", False)
        ]
    , mkTool "agda_get_context" "Get the context (available variables and their types) at a goal"
        [("goalId", "integer", "The numeric ID of the goal/hole (starting from 0)", True)
        ,("sessionId", "string", "Optional session ID for multi-agent isolation", False)
        ,("format", "string", "Response format: Concise (default) or Full", False)
        ]
    , mkTool "agda_get_context_implicits" "Get the context at a goal with implicit arguments shown"
        [("goalId", "integer", "The numeric ID of the goal/hole (starting from 0)", True)
        ,("sessionId", "string", "Optional session ID for multi-agent isolation", False)
        ,("format", "string", "Response format: Concise (default) or Full", False)
        ]
    , mkTool "agda_give" "Fill a goal/hole with an expression"
        [("goalId", "integer", "The numeric ID of the goal/hole", True)
        ,("expression", "string", "Agda expression to use", True)
        ,("sessionId", "string", "Optional session ID for multi-agent isolation", False)
        ,("format", "string", "Response format: Concise (default) or Full", False)
        ]
    , mkTool "agda_refine" "Refine a goal with a constructor or function, introducing new sub-goals"
        [("goalId", "integer", "The numeric ID of the goal/hole", True)
        ,("expression", "string", "Agda expression to refine with", True)
        ,("sessionId", "string", "Optional session ID for multi-agent isolation", False)
        ,("format", "string", "Response format: Concise (default) or Full", False)
        ]
    , mkTool "agda_case_split" "Split a goal by pattern matching on a variable"
        [("goalId", "integer", "The numeric ID of the goal/hole", True)
        ,("variable", "string", "Name of the variable to case-split on", True)
        ,("sessionId", "string", "Optional session ID for multi-agent isolation", False)
        ,("format", "string", "Response format: Concise (default) or Full", False)
        ]
    , mkTool "agda_compute" "Parse and display an expression in a goal's context"
        [("goalId", "integer", "The numeric ID of the goal/hole", True)
        ,("expression", "string", "Agda expression to compute", True)
        ,("sessionId", "string", "Optional session ID for multi-agent isolation", False)
        ,("format", "string", "Response format: Concise (default) or Full", False)
        ]
    , mkTool "agda_infer_type" "Infer the type of an expression in a goal's context"
        [("goalId", "integer", "The numeric ID of the goal/hole", True)
        ,("expression", "string", "Agda expression to infer type for", True)
        ,("sessionId", "string", "Optional session ID for multi-agent isolation", False)
        ,("format", "string", "Response format: Concise (default) or Full", False)
        ]
    , mkTool "agda_intro" "Introduce variables using the intro tactic"
        [("goalId", "integer", "The numeric ID of the goal/hole", True)
        ,("sessionId", "string", "Optional session ID for multi-agent isolation", False)
        ,("format", "string", "Response format: Concise (default) or Full", False)
        ]
    , mkTool "agda_auto" "Attempt automatic proof search to fill a goal"
        [("goalId", "integer", "The numeric ID of the goal/hole", True)
        ,("timeout", "integer", "Optional timeout in milliseconds", False)
        ,("sessionId", "string", "Optional session ID for multi-agent isolation", False)
        ,("format", "string", "Response format: Concise (default) or Full", False)
        ]
    , mkTool "agda_auto_all" "Attempt automatic proof search on all goals"
        [("timeout", "integer", "Optional timeout in milliseconds", False)
        ,("sessionId", "string", "Optional session ID for multi-agent isolation", False)
        ,("format", "string", "Response format: Concise (default) or Full", False)
        ]
    , mkTool "agda_solve_one" "Attempt to solve a specific goal using Agda's constraint solver"
        [("goalId", "integer", "The numeric ID of the goal/hole", True)
        ,("sessionId", "string", "Optional session ID for multi-agent isolation", False)
        ,("format", "string", "Response format: Concise (default) or Full", False)
        ]
    , mkTool "agda_helper_function" "Generate a helper function skeleton for a goal"
        [("goalId", "integer", "The numeric ID of the goal/hole", True)
        ,("helperName", "string", "Suggested name for the helper function", True)
        ,("sessionId", "string", "Optional session ID for multi-agent isolation", False)
        ,("format", "string", "Response format: Concise (default) or Full", False)
        ]
    , mkTool "agda_goal_type_context" "Get both the goal type and context together"
        [("goalId", "integer", "The numeric ID of the goal/hole", True)
        ,("sessionId", "string", "Optional session ID for multi-agent isolation", False)
        ,("format", "string", "Response format: Concise (default) or Full", False)
        ]
    , mkTool "agda_goal_at_position" "Find which goal exists at a specific file position"
        [("file", "string", "Path to the Agda file", True)
        ,("line", "integer", "Line number (1-indexed)", True)
        ,("column", "integer", "Column number (1-indexed)", True)
        ,("sessionId", "string", "Optional session ID for multi-agent isolation", False)
        ,("format", "string", "Response format: Concise (default) or Full", False)
        ]
    , mkTool "agda_goto_definition" "Navigate to the definition of a symbol at a specific position"
        [("file", "string", "Path to the Agda file", True)
        ,("line", "integer", "Line number (1-indexed)", True)
        ,("column", "integer", "Column number (1-indexed)", True)
        ,("sessionId", "string", "Optional session ID for multi-agent isolation", False)
        ,("format", "string", "Response format: Concise (default) or Full", False)
        ]
    , mkTool "agda_search_about" "Search for definitions by name or type signature"
        [("query", "string", "Search query", True)
        ,("sessionId", "string", "Optional session ID for multi-agent isolation", False)
        ,("format", "string", "Response format: Concise (default) or Full", False)
        ]
    , mkTool "agda_show_module" "Show the contents of a module"
        [("moduleName", "string", "Fully qualified module name", True)
        ,("sessionId", "string", "Optional session ID for multi-agent isolation", False)
        ,("format", "string", "Response format: Concise (default) or Full", False)
        ]
    , mkTool "agda_show_constraints" "Show all unsolved type-checking constraints"
        [("sessionId", "string", "Optional session ID for multi-agent isolation", False)
        ,("format", "string", "Response format: Concise (default) or Full", False)
        ]
    , mkTool "agda_why_in_scope" "Look up documentation and scope information for a name"
        [("name", "string", "Name to look up", True)
        ,("sessionId", "string", "Optional session ID for multi-agent isolation", False)
        ,("format", "string", "Response format: Concise (default) or Full", False)
        ]
    , mkTool "agda_list_postulates" "List all postulates in a file"
        [("file", "string", "Path to the Agda file", True)
        ,("sessionId", "string", "Optional session ID for multi-agent isolation", False)
        ,("format", "string", "Response format: Concise (default) or Full", False)
        ]
    ]

-- | Helper to create a tool definition
mkTool :: Text -> Text -> [(Text, Text, Text, Bool)] -> Tool
mkTool toolName desc params = Tool
    { name = toolName
    , title = Nothing
    , description = Just desc
    , inputSchema = InputSchema
        { schemaType = "object"
        , properties = Just $ Map.fromList
            [(pName, toJSON $ Map.fromList
                [ ("type" :: Text, JSON.String pType)
                , ("description", JSON.String pDesc)
                ]) | (pName, pType, pDesc, _) <- params]
        , required = Just [pName | (pName, _, _, True) <- params]
        }
    , outputSchema = Nothing
    , annotations = Nothing
    , _meta = Nothing
    }

-- | Define Agda resources
agdaResources :: [Resource]
agdaResources =
    [ Resource
        { uri = "agda://goals"
        , name = "goals"
        , title = Just "Agda Goals"
        , description = Just "List of all goals in the currently loaded Agda file"
        , mimeType = Just "application/json"
        , size = Nothing
        , annotations = Nothing
        , _meta = Nothing
        }
    , Resource
        { uri = "agda://file-context"
        , name = "file-context"
        , title = Just "File Context"
        , description = Just "Overall context and scope information for the loaded file"
        , mimeType = Just "application/json"
        , size = Nothing
        , annotations = Nothing
        , _meta = Nothing
        }
    ]

-- | Call an Agda tool based on name and arguments
callAgdaTool :: SessionManager.SessionManager ServerState -> Text -> Map.Map Text Value -> IO CallToolResult
callAgdaTool manager toolName args = do
    -- Parse tool and call handler
    mTool <- parseAgdaTool toolName args
    case mTool of
        Nothing -> return $ CallToolResult
            { content = [TextContentType $ TextContent "text" ("Unknown tool: " <> toolName) Nothing Nothing]
            , structuredContent = Nothing
            , isError = Just True
            , _meta = Nothing
            }
        Just tool -> do
            result <- handleAgdaToolWithSession manager tool
            return $ CallToolResult
                { content = [result]
                , structuredContent = Nothing
                , isError = Nothing
                , _meta = Nothing
                }

-- | Parse tool name and arguments to AgdaTool
parseAgdaTool :: Text -> Map.Map Text Value -> IO (Maybe AgdaTool)
parseAgdaTool toolName args = do
    let sessionId = getTextArg "sessionId" args
        format = getTextArg "format" args

    return $ case toolName of
        "agda_load" -> do
            file <- getTextArg "file" args
            Just $ AgdaLoad file sessionId format
        "agda_get_goals" ->
            Just $ AgdaGetGoals sessionId format
        "agda_get_goal_type" -> do
            goalId <- getIntArg "goalId" args
            Just $ AgdaGetGoalType goalId sessionId format
        "agda_get_goal_type_implicits" -> do
            goalId <- getIntArg "goalId" args
            Just $ AgdaGetGoalTypeImplicits goalId sessionId format
        "agda_get_context" -> do
            goalId <- getIntArg "goalId" args
            Just $ AgdaGetContext goalId sessionId format
        "agda_get_context_implicits" -> do
            goalId <- getIntArg "goalId" args
            Just $ AgdaGetContextImplicits goalId sessionId format
        "agda_give" -> do
            goalId <- getIntArg "goalId" args
            expression <- getTextArg "expression" args
            Just $ AgdaGive goalId expression sessionId format
        "agda_refine" -> do
            goalId <- getIntArg "goalId" args
            expression <- getTextArg "expression" args
            Just $ AgdaRefine goalId expression sessionId format
        "agda_case_split" -> do
            goalId <- getIntArg "goalId" args
            variable <- getTextArg "variable" args
            Just $ AgdaCaseSplit goalId variable sessionId format
        "agda_compute" -> do
            goalId <- getIntArg "goalId" args
            expression <- getTextArg "expression" args
            Just $ AgdaCompute goalId expression sessionId format
        "agda_infer_type" -> do
            goalId <- getIntArg "goalId" args
            expression <- getTextArg "expression" args
            Just $ AgdaInferType goalId expression sessionId format
        "agda_intro" -> do
            goalId <- getIntArg "goalId" args
            Just $ AgdaIntro goalId sessionId format
        "agda_auto" -> do
            goalId <- getIntArg "goalId" args
            let timeout = getIntArg "timeout" args
            Just $ AgdaAuto goalId timeout sessionId format
        "agda_auto_all" -> do
            let timeout = getIntArg "timeout" args
            Just $ AgdaAutoAll timeout sessionId format
        "agda_solve_one" -> do
            goalId <- getIntArg "goalId" args
            Just $ AgdaSolveOne goalId sessionId format
        "agda_helper_function" -> do
            goalId <- getIntArg "goalId" args
            helperName <- getTextArg "helperName" args
            Just $ AgdaHelperFunction goalId helperName sessionId format
        "agda_goal_type_context" -> do
            goalId <- getIntArg "goalId" args
            Just $ AgdaGoalTypeContext goalId sessionId format
        "agda_goal_at_position" -> do
            file <- getTextArg "file" args
            line <- getIntArg "line" args
            column <- getIntArg "column" args
            Just $ AgdaGoalAtPosition file line column sessionId format
        "agda_goto_definition" -> do
            file <- getTextArg "file" args
            line <- getIntArg "line" args
            column <- getIntArg "column" args
            Just $ AgdaGotoDefinition file line column sessionId format
        "agda_search_about" -> do
            query <- getTextArg "query" args
            Just $ AgdaSearchAbout query sessionId format
        "agda_show_module" -> do
            moduleName <- getTextArg "moduleName" args
            Just $ AgdaShowModule moduleName sessionId format
        "agda_show_constraints" ->
            Just $ AgdaShowConstraints sessionId format
        "agda_why_in_scope" -> do
            nameArg <- getTextArg "name" args
            Just $ AgdaWhyInScope nameArg sessionId format
        "agda_list_postulates" -> do
            file <- getTextArg "file" args
            Just $ AgdaListPostulates file sessionId format
        _ -> Nothing

-- | Get a text argument from the map
getTextArg :: Text -> Map.Map Text Value -> Maybe Text
getTextArg key args = case Map.lookup key args of
    Just (JSON.String s) -> Just s
    _ -> Nothing

-- | Get an integer argument from the map
getIntArg :: Text -> Map.Map Text Value -> Maybe Int
getIntArg key args = case Map.lookup key args of
    Just (JSON.Number n) -> Just (floor n)
    _ -> Nothing

-- | Read an Agda resource
readAgdaResource :: SessionManager.SessionManager ServerState -> Text -> IO ReadResourceResult
readAgdaResource _manager _resourceUri = do
    -- For now, return empty result - resources need more implementation
    return $ ReadResourceResult
        { contents = []
        , _meta = Nothing
        }

main :: IO ()
main = do
    -- Set UTF-8 encoding for proper Unicode handling
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8

    hPutStrLn stderr "Starting Agda MCP Server on http://localhost:3000/mcp"
    hPutStrLn stderr "Session isolation enabled: pass 'sessionId' parameter for multi-agent support"

    -- Initialize session manager
    sessionManager <- initSessionManager
    setGlobalSessionManager sessionManager

    let serverInfo = Implementation
            { name = "Agda MCP Server"
            , title = Just "Agda MCP Server"
            , version = "1.0.0"
            }

    let resourcesCap = ResourcesCapability
            { subscribe = Just False
            , listChanged = Just False
            }
    let promptsCap = PromptsCapability
            { listChanged = Just False
            }
    let toolsCap = ToolsCapability
            { listChanged = Just False
            }

    let capabilities = ServerCapabilities
            { resources = Just resourcesCap
            , prompts = Just promptsCap
            , tools = Just toolsCap
            , completions = Nothing
            , logging = Nothing
            , experimental = Nothing
            }

    let config = HTTPServerConfig
            { httpPort = 3000
            , httpBaseUrl = "http://localhost:3000"
            , httpServerInfo = serverInfo
            , httpCapabilities = capabilities
            , httpEnableLogging = True
            , httpOAuthConfig = Nothing
            , httpJWK = Nothing
            , httpProtocolVersion = mcpProtocolVersion
            }

    runServerHTTP config
