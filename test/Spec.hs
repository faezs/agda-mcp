module Main (main) where

import Test.Tasty
import qualified AgdaMCP.ServerSpec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Agda MCP Server Tests"
  [ AgdaMCP.ServerSpec.tests
  ]
