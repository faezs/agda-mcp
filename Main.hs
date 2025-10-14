{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import MCP.Server
import MCP.Server.Derive
import System.IO (hSetEncoding, stderr, stdout, utf8, hPutStrLn)

import AgdaMCP.Server
import AgdaMCP.Types

main :: IO ()
main = do
  -- Set UTF-8 encoding for proper Unicode handling
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  hPutStrLn stderr "Starting Agda MCP Server on http://localhost:3000/mcp"

  -- Initialize server state
  stateRef <- initServerState

  -- Define handlers that close over stateRef
  let handleTool :: AgdaTool -> IO Content
      handleTool = handleAgdaTool stateRef

      handleResource :: URI -> AgdaResource -> IO ResourceContent
      handleResource = handleAgdaResource stateRef

      -- Derive MCP handlers using Template Haskell
      tools = $(deriveToolHandlerWithDescription ''AgdaTool 'handleTool agdaToolDescriptions)
      resources = $(deriveResourceHandlerWithDescription ''AgdaResource 'handleResource agdaResourceDescriptions)

   in -- Run the MCP server with HTTP transport
      runMcpServerHttp
        McpServerInfo
          { serverName = "Agda MCP Server"
          , serverVersion = "1.0.0"
          , serverInstructions = "A Model Context Protocol server for interactive Agda development. Provides tools for loading files, working with goals/holes, refining proofs, and exploring scope."
          }
        McpServerHandlers
          { prompts = Nothing  -- No prompts defined yet
          , resources = Just resources
          , tools = Just tools
          }
