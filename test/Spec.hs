{-# LANGUAGE TypeApplications #-}
import Test.Fluent.Assertions
import Test.Fluent.Tasty.TestCase
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

data Foo = Foo {name :: String, age :: Int} deriving (Show, Eq)

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ testGroup
        "Unit tests"
        $ [ fluentTestCase "test fluent1" $
              assertThat @String "1    " $
                isEqualTo ""
                  . isEqualTo "1    "
                  . tag "length"
                  . focus length
                  . isEqualTo 50000
                  . isEqualTo 1
                  . tag "dupa"
                  . isGreaterThan 1000
                  . isLowerThan 3
          ],
      fluentTestCase "sadf" $
        assertThat [1, 2, 3, 5, 65] $
          isEqualTo [11, 2, 3, 5, 65]
    ]