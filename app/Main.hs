module Main where

import Lib 
import Test.Tasty

main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ fluentTestCase "test fluent" $
       assertThat "1" $ isEqualTo "" . isEqualTo "1" . isEqualTo "2"
  ]