module Main (main) where

import Test.Tasty
import Test.Tasty.Options (OptionDescription(..), lookupOption)
import Test.Tasty.Runners (NumThreads(..))
import qualified AgdaMCP.ServerSpec
import qualified AgdaMCP.MultiAgentSpec

main :: IO ()
main = defaultMain $ localOption (NumThreads 1) tests

tests :: TestTree
tests = testGroup "Agda MCP Server Tests"
  [ AgdaMCP.ServerSpec.tests
  , AgdaMCP.MultiAgentSpec.tests
  ]
