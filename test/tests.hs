import Test.Tasty

import ConnegTests
import ParserTests
import ResourceTests

main = defaultMain tests

tests :: TestTree
tests = testGroup "Webcrank tests" 
  [ connegTests
  , parserTests
  , runResourceTests
  ]

