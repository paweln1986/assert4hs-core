{-# LANGUAGE TypeApplications #-}

import Test.Fluent.Assertions
  ( assertThat,
    focus,
    inside,
    isEqualTo,
    isGreaterThan,
    isLowerThan,
    tag,
  )
import Test.Fluent.Tasty.TestCase (fluentTestCase)
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
        assertThat (Foo "someName" 15) $
          tag "foo"
            . isEqualTo (Foo "someN1ame" 15)
            . inside age (tag "age" . isGreaterThan 20 . isLowerThan 10)
            . isEqualTo (Foo "someName" 15)
      , fluentTestCase "sadf" $
        assertThat 15 $ isEqualTo 16
    ]