module Main (main) where

import Test.Tasty
import qualified AgdaMCP.ServerSpec
import qualified AgdaMCP.MultiAgentSpec
import qualified AgdaMCP.EditPersistenceSpec
import qualified AgdaMCP.Autoformalizer.ProtocolSpec
import qualified AgdaMCP.Autoformalizer.MarcolliManinSpec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Agda MCP Server Tests"
  [ AgdaMCP.ServerSpec.tests
  , AgdaMCP.MultiAgentSpec.tests
  , AgdaMCP.EditPersistenceSpec.tests
  , AgdaMCP.Autoformalizer.ProtocolSpec.tests
  , AgdaMCP.Autoformalizer.MarcolliManinSpec.tests
  ]
