{-# LANGUAGE OverloadedStrings #-}
module Main where

import AgdaMCP.Server
import System.Environment (getArgs, getProgName)
import System.IO (stderr, hPutStrLn)

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName

  case args of
    [] -> do
      hPutStrLn stderr $ "Starting " ++ progName ++ " MCP server..."
      runAgdaMCPServer
    ["--help"] -> putStrLn helpText
    _ -> hPutStrLn stderr "Usage: agda-mcp [--help]"

helpText :: String
helpText = unlines
  [ "Agda MCP Server - Model Context Protocol server for Agda interaction"
  , ""
  , "Provides the following tools:"
  , "  agda_get_goals      - Get all goals/holes in a file"
  , "  agda_get_goal_type  - Get the type of a specific goal"
  , "  agda_get_context    - Get context at a goal"
  , "  agda_give           - Fill a goal with an expression"
  , "  agda_refine         - Refine a goal with constructor/function"
  , "  agda_auto           - Try automatic proof search"
  , "  agda_normalize      - Normalize an expression"
  , "  agda_infer_type     - Infer type of expression"
  ]