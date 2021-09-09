{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      : Test.Fluent.Assertions
-- Description : Set of combinators and primitives to use fluen assertions
-- Copyright   : (c) Pawel Nosal, 2021
-- License     : MIT
-- Maintainer  : p.nosal1986@gmail.com
-- Stability   : experimental
--
-- This library aims to provide a set of combinators to assert arbitrary nested data structures.
-- The inspiration of this library is AssertJ for Java, the composition of assertions was inspired by `lens` library.
--
-- Example:
--
-- @
--  data Foo = Foo {name :: String, age :: Int} deriving (Show, Eq)
--
--  assertThat (Foo "someName" 15) $
--       isEqualTo (Foo "someN1ame" 15)
--       . focus age
--       . tag "age"
--       . isGreaterThan 20
-- @
--
-- result in
--
-- @
--  given Foo {name = "someName", age = 15} should be equal to Foo {name = "someN1ame", age = 15}
--  Foo {name = "someName", age = 15}
--  ╷
--  │
--  ╵
--  Foo {name = "someN1ame", age = 15}
--                    ▲
--  [age] given 15 should be greater than 20
-- @
module Test.Fluent.Assertions
  ( -- * Assertions

    -- ** Basic assertions
    simpleAssertion,
    isEqualTo,
    isNotEqualTo,
    isGreaterThan,
    isGreaterEqualThan,
    isLowerThan,
    isLowerEqualThan,
    shouldSatisfy,
    hasSize,
    isEmpty,
    isNotEmpty,
    contains,

    -- ** Assertion util functions
    focus,
    inside,
    tag,
    forceError,

    -- ** Assertion configuration
    AssertionConfig,
    defaultConfig,
    setAssertionTimeout,

    -- * Types

    -- ** Assertion defitions
    Assertion,
    Assertion',

    -- ** Assertion failure
    FluentTestFailure (..),
  )
where

import Data.Functor.Contravariant (Contravariant (contramap))
import GHC.Stack (HasCallStack)
import Test.Fluent.Diff (pretty)
import Test.Fluent.Internal.AssertionConfig
  ( AssertionConfig,
    defaultConfig,
    setAssertionTimeout,
  )
import Test.Fluent.Internal.Assertions (Assertion, Assertion', AssertionDefinition (SequentialAssertions), FluentTestFailure (..), basicAssertion, transformAssertions, updateLabel)

-- | The 'simpleAssertion' function is a building block of more complicated assertions.
--
--  It takes one predicate and function to format an error message.
--
-- @
--  myIsEqual x = simpleAssertion (== x) (\\x' -> show x' <> " is not equal to " <> show x)
-- @
simpleAssertion ::
  HasCallStack =>
  -- | A predicate that should be met by the subject under test
  (a -> Bool) ->
  -- | A function that allows formatting an error message once the predicate is not met
  (a -> String) ->
  Assertion a
simpleAssertion predicate formatter f s = basicAssertion predicate formatter (f s)

-- | assert if the subject under test is equal to the given value
--
-- @
--  assertThat 15 $ isEqualTo 16
-- @
--
-- result
--
-- @
--  given 15 should be equal to 16
--   ▼
--  15
--  ╷
--  │
--  ╵
--  16
--   ▲
-- @
isEqualTo :: (Eq a, Show a, HasCallStack) => a -> Assertion a
isEqualTo a = simpleAssertion (a ==) (formatMessage True "should be equal to" a)

isNotEqualTo :: (Eq a, Show a, HasCallStack) => a -> Assertion a
isNotEqualTo a = simpleAssertion (a /=) (formatMessage False "should be not equal to" a)

-- | assert if the subject under test is greater than given value
--
-- @
--  assertThat 15 $ isGreaterThan 16
-- @
--
-- result
--
-- @
--  given 15 should be greater than 16
-- @
isGreaterThan :: (Ord a, Show a, HasCallStack) => a -> Assertion a
isGreaterThan a = simpleAssertion (a <) (formatMessage False "should be greater than" a)

isGreaterEqualThan :: (Ord a, Show a, HasCallStack) => a -> Assertion a
isGreaterEqualThan a = simpleAssertion (a <=) (formatMessage False "should be greater or equal to" a)

-- | assert if the subject under test is lower than given value
--
-- @
--  assertThat 16 $ isLowerThan 15
-- @
--
-- result
--
-- @
--  given 16 should be lower than 15
-- @
isLowerThan :: (Ord a, Show a, HasCallStack) => a -> Assertion a
isLowerThan a = simpleAssertion (a >) (formatMessage False "should be lower than" a)

isLowerEqualThan :: (Ord a, Show a, HasCallStack) => a -> Assertion a
isLowerEqualThan a = simpleAssertion (a >=) (formatMessage False "should be lower or equal to" a)

shouldSatisfy :: (Show a, HasCallStack) => (a -> Bool) -> Assertion a
shouldSatisfy predicate = simpleAssertion predicate (formatMessage' "does not met a predicate")

hasSize :: (Foldable t, HasCallStack) => Int -> Assertion (t a)
hasSize expectedSize = inside length (simpleAssertion (== expectedSize) assertionMessage)
  where
    assertionMessage currentSize = "expected size " <> show expectedSize <> " is not equal actual size " <> show currentSize

isEmpty :: (Foldable t, HasCallStack) => Assertion (t a)
isEmpty = inside null (simpleAssertion (== True) assertionMessage)
  where
    assertionMessage _ = "should be empty, but is not"

isNotEmpty :: (Foldable t, HasCallStack) => Assertion (t a)
isNotEmpty = inside null (simpleAssertion (== False) assertionMessage)
  where
    assertionMessage _ = "should be not empty"

contains :: (Foldable t, Eq a, Show a, HasCallStack) => a -> Assertion (t a)
contains expectedElem = inside (elem expectedElem) (simpleAssertion (== True) assertionMessage)
  where
    assertionMessage _ = "should contain element " <> show expectedElem <> ", but it doesn't"

-- | allow changing subject under test using a transformation function
--
-- @
--  assertThat "1    " $
--            isNotEqualTo ""
--              . focus length
--              . isEqualTo 10
-- @
--
-- result
--
-- @
--  given 5 should be equal to 10
--  ▼
--  5
--  ╷
--  │
--  ╵
--  10
--  ▲▲
-- @
focus :: (a -> b) -> Assertion' a b
focus f assert s = contramap f (assert (f s))

-- |  like 'focus', this function allow changing subject under test, it takes an assertion for modified value, then it allows us to continue assertion on the original value
--
-- @
--   assertThat (Foo "someName" 15) $
--                 isEqualTo (Foo "someN1ame" 15)
--               . inside age (tag "age" . isGreaterThan 20 . isLowerThan 10)
--               . isEqualTo (Foo "someName" 15)
-- @
--
-- result
--
-- @
--  given Foo {name = "someName", age = 15} should be equal to Foo {name = "someN1ame", age = 15}
--        Foo {name = "someName", age = 15}
--        ╷
--        │
--        ╵
--        Foo {name = "someN1ame", age = 15}
--                          ▲
--        [age] given 15 should be greater than 20
--        [age] given 15 should be lower than 10
-- @
inside :: (b -> a) -> Assertion a -> Assertion b
inside f assert' b s = b s <> mconcat (transformAssertions [assert' mempty (f s)] f)

-- |  this combinator allows marking the following assertion with a given prefix
--
-- @
-- assertThat (Foo "someName" 15) $
--   tag "foo" . isEqualTo (Foo "someN1ame" 15)
--     . inside age (tag "age" . isGreaterThan 20 . isLowerThan 10)
--     . tag "foo not equal"
--     . isNotEqualTo (Foo "someName" 15)
-- @
--
-- result
--
-- @
--  [foo] given Foo {name = "someName", age = 15} should be equal to Foo {name = "someN1ame", age = 15}
--  Foo {name = "someName", age = 15}
--  ╷
--  │
--  ╵
--  Foo {name = "someN1ame", age = 15}
--                    ▲
--  [foo.age] given 15 should be greater than 20
--  [foo.age] given 15 should be lower than 10
--  [foo.not equal to] given Foo {name = "someName", age = 15} should be not equal to Foo {name = "someName", age = 15}
--  Foo {name = "someName", age = 15}
--  ╷
--  │
--  ╵
--  Foo {name = "someName", age = 15}
-- @
tag :: String -> Assertion a
tag label assert s = updateLabel label (assert s)

-- |  Sometimes it is handy to stop the assertions chain.
--
--    This combinator gets an assertion that should be forced, any following assertion will be not executed then
--
-- @
-- extracting :: HasCallStack => Assertion' (Maybe a) a
-- extracting = forceError isJust . focus Maybe.fromJust
-- @
forceError :: Assertion a -> Assertion a
forceError assert' b s = SequentialAssertions [b s] <> mconcat (transformAssertions [assert' mempty s] id)

formatMessage :: Show a => Bool -> String -> a -> a -> String
formatMessage True message a a' = "given " <> show a' <> " " <> message <> " " <> show a <> "\n" <> pretty a' a <> "\n"
formatMessage False message a a' = "given " <> show a' <> " " <> message <> " " <> show a

formatMessage' :: Show a => String -> a -> String
formatMessage' message a = "given " <> show a <> " " <> message
