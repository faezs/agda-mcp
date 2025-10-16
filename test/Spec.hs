module Main (main) where

import Test.Tasty
import qualified AgdaMCP.ServerSpec
import qualified AgdaMCP.EditPersistenceSpec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Agda MCP Server Tests"
  [ AgdaMCP.ServerSpec.tests
  , AgdaMCP.EditPersistenceSpec.tests
  ]
