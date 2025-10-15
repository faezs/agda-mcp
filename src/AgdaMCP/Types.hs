{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module AgdaMCP.Types where

import Data.Text (Text)
import qualified Data.Aeson as JSON
import GHC.Generics (Generic)

-- Response format options for controlling output verbosity
data ResponseFormat
    = Concise  -- Human-readable, compact format (default)
    | Full     -- Complete JSON with ranges for programmatic processing
    deriving (Show, Eq, Generic)

-- JSON instances for MCP protocol
instance JSON.ToJSON ResponseFormat
instance JSON.FromJSON ResponseFormat

-- Tool definitions for Agda MCP server
-- These will be automatically converted to MCP tool schemas via Template Haskell
-- Note: format is Maybe Text for TH compatibility, parsed to ResponseFormat in handlers

data AgdaTool
    = AgdaLoad { file :: Text, format :: Maybe Text }
    | AgdaGetGoals { format :: Maybe Text }
    | AgdaGetGoalType { goalId :: Int, format :: Maybe Text }
    | AgdaGetGoalTypeImplicits { goalId :: Int, format :: Maybe Text }
    | AgdaGetContext { goalId :: Int, format :: Maybe Text }
    | AgdaGetContextImplicits { goalId :: Int, format :: Maybe Text }
    | AgdaGive { goalId :: Int, expression :: Text, format :: Maybe Text }
    | AgdaRefine { goalId :: Int, expression :: Text, format :: Maybe Text }
    | AgdaCaseSplit { goalId :: Int, variable :: Text, format :: Maybe Text }
    | AgdaCompute { goalId :: Int, expression :: Text, format :: Maybe Text }
    | AgdaInferType { goalId :: Int, expression :: Text, format :: Maybe Text }
    | AgdaIntro { goalId :: Int, format :: Maybe Text }
    | AgdaAuto { goalId :: Int, timeout :: Maybe Int, format :: Maybe Text }
    | AgdaAutoAll { timeout :: Maybe Int, format :: Maybe Text }
    | AgdaSolveOne { goalId :: Int, format :: Maybe Text }
    | AgdaHelperFunction { goalId :: Int, helperName :: Text, format :: Maybe Text }
    | AgdaGoalTypeContext { goalId :: Int, format :: Maybe Text }
    | AgdaSearchAbout { query :: Text, format :: Maybe Text }
    | AgdaShowModule { moduleName :: Text, format :: Maybe Text }
    | AgdaShowConstraints { format :: Maybe Text }
    | AgdaWhyInScope { name :: Text, format :: Maybe Text }
    | AgdaListPostulates { file :: Text, format :: Maybe Text }
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
    , ("AgdaGetGoalTypeImplicits", "Get the type of a specific goal with implicit arguments shown")
    , ("AgdaGetContext", "Get the context (available variables and their types) at a goal")
    , ("AgdaGetContextImplicits", "Get the context at a goal with implicit arguments shown")
    , ("AgdaGive", "Fill a goal/hole with an expression")
    , ("AgdaRefine", "Refine a goal with a constructor or function, introducing new sub-goals")
    , ("AgdaCaseSplit", "Split a goal by pattern matching on a variable")
    , ("AgdaCompute", "Parse and display an expression in a goal's context")
    , ("AgdaInferType", "Infer the type of an expression in a goal's context")
    , ("AgdaIntro", "Introduce variables using the intro tactic (generates suggestions)")
    , ("AgdaAuto", "Attempt automatic proof search to fill a goal (uses Agda's Auto/Agsy tactic)")
    , ("AgdaAutoAll", "Attempt automatic proof search on all goals in the file (batch auto operation)")
    , ("AgdaSolveOne", "Attempt to solve a specific goal using Agda's constraint solver")
    , ("AgdaHelperFunction", "Generate a helper function skeleton for a goal (useful for refactoring)")
    , ("AgdaGoalTypeContext", "Get both the goal type and context together (more efficient than separate calls)")
    , ("AgdaSearchAbout", "Search for definitions by name or type signature (Hoogle-style search)")
    , ("AgdaShowModule", "Show the contents of a module (all exported definitions, types, and submodules)")
    , ("AgdaShowConstraints", "Show all unsolved type-checking constraints in the current file")
    , ("AgdaWhyInScope", "Look up documentation and scope information for a name")
    , ("AgdaListPostulates", "List all postulates in a file with their names, types, and positions. Useful for converting postulates to holes for implementation.")
    , ("file", "Path to the Agda file")
    , ("moduleName", "Fully qualified module name (e.g., 'Data.Nat', 'Agda.Builtin.Nat')")
    , ("goalId", "The numeric ID of the goal/hole (starting from 0)")
    , ("expression", "Agda expression to use (e.g., 'zero', 'suc n', 'refl')")
    , ("variable", "Name of the variable to case-split on")
    , ("query", "Search query: can be a name pattern or type signature to search for")
    , ("name", "Name to look up in scope")
    , ("helperName", "Suggested name for the generated helper function")
    , ("timeout", "Optional timeout in milliseconds for proof search (default: 5000)")
    , ("format", "Response format: \"Concise\" (default, human-readable, ~90% smaller) or \"Full\" (complete JSON with ranges for programmatic processing)")
    ]

-- Human-readable descriptions for resources
agdaResourceDescriptions :: [(String, String)]
agdaResourceDescriptions =
    [ ("Goals", "List of all goals in an Agda file (URI: resource://goals/{file})")
    , ("GoalInfo", "Detailed information about a specific goal (URI: resource://goal_info/{file}/{id})")
    , ("FileContext", "Overall context and scope information for a file (URI: resource://file_context/{file})")
    ]
