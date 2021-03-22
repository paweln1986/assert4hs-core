{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Exception (Exception, throwIO)
import Test.Fluent.Assertions
import Test.Fluent.Assertions.Exceptions
  ( assertThrowing,
    exceptionOfType,
  )
import TestCase (fluentTestCase)
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

data Foo = Foo {name :: String, age :: Int} deriving (Show, Eq)

data MyException = ThisException | ThatException
  deriving (Show)

instance Exception MyException

data MyException1 = ThisException1 | ThatException1
  deriving (Show)

instance Exception MyException1

isSuitableForEmployment :: Assertion Foo
isSuitableForEmployment =
  simpleAssertion (\a -> age a > 17) (\a -> "new employee must be 18 years or older, but it has " <> show (age a))
    . simpleAssertion (\a -> age a < 70) (\a -> "must be younger than 70 years old, but it has " <> show (age a))

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ testGroup
        "Unit tests"
        [],
      -- fluentTestCase "test fluent1" $
      -- assertThat @String "1    " $ isNotEqualTo "" . focus length . isEqualTo 10 . tag "sdfadasdaf"

      -- fluentTestCase "sadf" $
      --   assertThat (Foo "someName" 15) $
      --     tag "foo" . isEqualTo (Foo "someN1ame" 15)
      --       . inside age (tag "age" . isGreaterThan 20 . isLowerThan 10)
      --       . tag "not equal to"
      --       . isNotEqualTo (Foo "someName" 15),
      -- fluentTestCase "sadf" $
      --   assertThrowing (throwIO ThatException) (exceptionOfType @MyException1) $
      --     shouldSatisfy
      --       ( \case
      --           ThisException1 -> True
      --       ),

      fluentTestCase "chaining assertions" $ do
        let result = 4
        assertThat result $
          isGreaterThan 5
            . isLowerThan 20,
      fluentTestCase "focusing on part of data structure" $ do
        assertThat (Foo "someName" 15) $
          isEqualTo (Foo "someName" 15)
            . focus age
            . isGreaterThan 20
            . isLowerEqualThan 5,
      fluentTestCase "Changing subject uder test" $ do
        assertThat (Foo "someName" 15) $
          inside age (isGreaterThan 20 . isLowerEqualThan 5)
            . focus name
            . isEqualTo "someName1",
      fluentTestCase "Tagging assertions" $ do
        assertThat (Foo "someName" 15) $
          inside age (tag "age" . isGreaterThan 20 . isLowerEqualThan 5)
            . tag "name"
            . focus name
            . isEqualTo "someName1"
            . tag "should not be equal"
            . isNotEqualTo "someName",
      fluentTestCase "Custom assertions" $ do
        assertThat (Foo "someName" 15) isSuitableForEmployment,
        fluentTestCase "Custom assertions" $ do
        assertThat (Foo "someName" 76) isSuitableForEmployment
    ]
