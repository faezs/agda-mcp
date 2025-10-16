{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

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
  hPutStrLn stderr "Session isolation enabled: pass 'sessionId' parameter for multi-agent support"

  -- Initialize session manager (replaces single server state)
  sessionManager <- initSessionManager

  -- Define handlers that close over sessionManager
  let handleTool :: AgdaTool -> IO Content
      handleTool = handleAgdaToolWithSession sessionManager

      handleResource :: URI -> AgdaResource -> IO ResourceContent
      handleResource = handleAgdaResourceWithSession sessionManager

      -- Derive MCP handlers using Template Haskell
      tools = $(deriveToolHandlerWithDescription ''AgdaTool 'handleTool agdaToolDescriptions)
      resources = $(deriveResourceHandlerWithDescription ''AgdaResource 'handleResource agdaResourceDescriptions)

   in -- Run the MCP server with HTTP transport
      runMcpServerHttp
        McpServerInfo
          { serverName = "Agda MCP Server"
          , serverVersion = "1.0.0"
          , serverInstructions = "A Model Context Protocol server for interactive Agda development. Provides tools for loading files, working with goals/holes, refining proofs, and exploring scope. Supports multi-agent isolation via sessionId parameter."
          }
        McpServerHandlers
          { prompts = Nothing  -- No prompts defined yet
          , resources = Just resources
          , tools = Just tools
          }
