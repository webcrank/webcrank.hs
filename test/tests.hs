import Test.Tasty

import ParserTests
import DecisionTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Webcrank tests"
  [ parserTests
  , decisionTests
  ]

