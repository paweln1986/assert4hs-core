module Main where

import Lib
  ( assertThat,
    fluentTestCase,
    focus,
    isEqualTo,
    isGreaterThan,
    isLowerThan,
    tag,
  )
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ fluentTestCase "test fluent1" $
        assertThat "1    " $
          isEqualTo ""
            . isEqualTo "1    "
            . tag "length"
            . focus length
            . isEqualTo 7
            . isEqualTo 1
            . tag "dupa"
            . isGreaterThan 1000
            . isLowerThan 3
    ]