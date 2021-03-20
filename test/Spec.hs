{-# LANGUAGE TypeApplications #-}

import Test.Fluent.Assertions
  ( assertThat,
    focus,
    hasSize,
    inside,
    isEmpty,
    isEqualTo,
    isGreaterThan,
    isLowerThan,
    isNotEqualTo,
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
        [ fluentTestCase "test fluent1" $
            assertThat @String "1    " $ isNotEqualTo "" . focus length . isEqualTo 10 . tag "sdfadasdaf"
        ],
      fluentTestCase "sadf" $
        assertThat (Foo "someName" 15) $
          tag "foo" . isEqualTo (Foo "someN1ame" 15)
            . inside age (tag "age" . isGreaterThan 20 . isLowerThan 10)
            . tag "not equal to"
            . isNotEqualTo (Foo "someName" 15),
      fluentTestCase "sadf" $
        assertThat [16 :: Integer] $ hasSize 10 . isEmpty
    ]