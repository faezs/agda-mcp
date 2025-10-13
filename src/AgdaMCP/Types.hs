{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Types where

import Data.Text (Text)

-- Tool definitions for Agda MCP server
-- These will be automatically converted to MCP tool schemas via Template Haskell

data AgdaTool
    = AgdaLoad { file :: Text }
    | AgdaGetGoals
    | AgdaGetGoalType { goalId :: Int }
    | AgdaGetContext { goalId :: Int }
    | AgdaGive { goalId :: Int, expression :: Text }
    | AgdaRefine { goalId :: Int, expression :: Text }
    | AgdaCaseSplit { goalId :: Int, variable :: Text }
    | AgdaCompute { goalId :: Int, expression :: Text }
    | AgdaInferType { goalId :: Int, expression :: Text }
    | AgdaIntro { goalId :: Int }
    | AgdaWhyInScope { name :: Text }
    deriving (Show, Eq)

-- Resource definitions for exposing Agda file information
-- Resources must be simple constructors with no parameters
-- Parameters are extracted from the resource URI instead
data AgdaResource
    = Goals
    | GoalInfo
    | FileContext
    deriving (Show, Eq)

-- Human-readable descriptions for tools and their parameters
agdaToolDescriptions :: [(String, String)]
agdaToolDescriptions =
    [ ("AgdaLoad", "Load and type-check an Agda file")
    , ("AgdaGetGoals", "List all goals/holes in the currently loaded file")
    , ("AgdaGetGoalType", "Get the type of a specific goal")
    , ("AgdaGetContext", "Get the context (available variables and their types) at a goal")
    , ("AgdaGive", "Fill a goal/hole with an expression")
    , ("AgdaRefine", "Refine a goal with a constructor or function, introducing new sub-goals")
    , ("AgdaCaseSplit", "Split a goal by pattern matching on a variable")
    , ("AgdaCompute", "Parse and display an expression in a goal's context")
    , ("AgdaInferType", "Infer the type of an expression in a goal's context")
    , ("AgdaIntro", "Introduce variables using the intro tactic (generates suggestions)")
    , ("AgdaWhyInScope", "Look up documentation and scope information for a name")
    , ("file", "Path to the Agda file")
    , ("goalId", "The numeric ID of the goal/hole (starting from 0)")
    , ("expression", "Agda expression to use (e.g., 'zero', 'suc n', 'refl')")
    , ("variable", "Name of the variable to case-split on")
    , ("name", "Name to look up in scope")
    ]

-- Human-readable descriptions for resources
agdaResourceDescriptions :: [(String, String)]
agdaResourceDescriptions =
    [ ("Goals", "List of all goals in an Agda file (URI: resource://goals/{file})")
    , ("GoalInfo", "Detailed information about a specific goal (URI: resource://goal_info/{file}/{id})")
    , ("FileContext", "Overall context and scope information for a file (URI: resource://file_context/{file})")
    ]
