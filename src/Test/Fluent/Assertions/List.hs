{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      : Test.Fluent.Assertions.List
-- Description : Set of assertions for List type
-- Copyright   : (c) Pawel Nosal, 2021
-- License     : MIT
-- Maintainer  : p.nosal1986@gmail.com
-- Stability   : experimental
--
-- This library aims to provide a set of combinators to assert List type.
module Test.Fluent.Assertions.List where

import Data.List (isPrefixOf)
import GHC.Stack (HasCallStack)
import Test.Fluent.Assertions (Assertion, forceError, simpleAssertion)
import Test.Fluent.Diff (pretty)

-- | assert if a given list has the same length as the expected list
--
-- @
--  assertThat [1..10] $ shouldHaveSameSizeAs [0..10]
-- @
shouldHaveSameSizeAs :: HasCallStack => [a] -> Assertion [a]
shouldHaveSameSizeAs expected = simpleAssertion predicate message
  where
    predicate given = length given == length expected
    message given =
      "should have same leght, but they don't. Given: "
        <> show (length given)
        <> " is not equal to expected "
        <> show (length expected)

-- | verify if the given list has a length lower or equal to an expected value
--
-- @
--  assertThat [1..10] $ shouldHaveSizeLowerOrEqual 10
-- @
shouldHaveSizeLowerOrEqual :: HasCallStack => Int -> Assertion [a]
shouldHaveSizeLowerOrEqual expected = simpleAssertion predicate message
  where
    predicate given = length given >= expected
    message given =
      "the lenght of given list is "
        <> show (length given)
        <> ", but should be lower or equal "
        <> show expected

-- | verify if the given list has an expected prefix
--
-- @
--  assertThat [1..10] $ shouldStartWith [0..4]
-- @
shouldStartWith :: (Eq a, Show a, HasCallStack) => [a] -> Assertion [a]
shouldStartWith expected = forceError (shouldHaveSizeLowerOrEqual expectedLenght) . simpleAssertion predicate message
  where
    predicate = (expected `isPrefixOf`)
    expectedLenght = length expected
    actualPrefix x = take expectedLenght x
    message x =
      "should start with "
        <> show expected
        <> ", but it start with "
        <> show (actualPrefix x)

-- | verify if the given list does not start with a given prefix
--
-- @
--  assertThat [1..10] $ shouldNotStartWith [1..4]
-- @
shouldNotStartWith :: (Eq a, Show a, HasCallStack) => [a] -> Assertion [a]
shouldNotStartWith expected = simpleAssertion predicate message
  where
    predicate = not . (expected `isPrefixOf`)
    message _ =
      "should not start with "
        <> show expected
        <> ", but it does"

shouldBeSameAs :: (Eq a, HasCallStack, Show a) => [a] -> Assertion [a]
shouldBeSameAs expected = simpleAssertion predicate message
  where
    predicate = (== expected)
    message given = "given list should be same as expected list, but is not.\n" <> pretty given expected

shouldContain :: (Eq a, HasCallStack, Show a) => a -> Assertion [a]
shouldContain expected = simpleAssertion predicate message
  where
    predicate = elem expected
    message _ = "given list should contain element " <> show expected <> ", but it doesn't."

shouldNotContain :: (Eq a, HasCallStack, Show a) => a -> Assertion [a]
shouldNotContain expected = simpleAssertion predicate message
  where
    predicate = notElem expected
    message _ = "given list should not contain element " <> show expected <> ", but it doesn."

-- | verify if the given list contains the same elements as the expected list in any order
--
-- @
--  assertThat [1..10] $ shouldNotStartWith [1..4]
-- @
shouldHaveSameElements :: (HasCallStack, Eq a, Show a) => [a] -> Assertion [a]
shouldHaveSameElements expected = forceError (shouldHaveSameSizeAs expected) . simpleAssertion predicate errorMessage
  where
    predicate given = all (`elem` expected) given
    inGiven given = filter (`notElem` given) expected
    inExpected given = filter (`notElem` expected) given
    errorMessage given = "two lists should have same elements but:\n " ++ message
      where
        message = case (inGiven given, inExpected given) of
          ([], []) -> "bug in shouldHaveSameElements assertion, please report this to the maintainer"
          (xs, ys) -> "given list don't have " ++ show xs ++ "\nexpected list don't have " ++ show ys
