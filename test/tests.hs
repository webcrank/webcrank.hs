import Test.Tasty

import DecisionTests
import ParserTests
import HandleRequestTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Webcrank tests"
  [ parserTests
  , decisionTests
  , handleRequestTests
  ]

