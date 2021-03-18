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
--     data Foo = Foo {name :: String, age :: Int} deriving (Show, Eq)
--
--     assertThat (Foo "someName" 15) $
--          isEqualTo (Foo "someN1ame" 15)
--          . focus age
--          . tag "age"
--          . isGreaterThan 20
-- @
--
-- result in
--
-- @
--      given Foo {name = "someName", age = 15} should be equal to Foo {name = "someN1ame", age = 15}
--      Foo {name = "someName", age = 15}
--      ╷
--      │
--      ╵
--      Foo {name = "someN1ame", age = 15}
--                        ▲
--      [age] given 15 should be greater than 20
-- @
module Test.Fluent.Assertions
  ( -- * Assertions

    -- ** Basic assertions
    simpleAssertion,
    isEqualTo,
    isGreaterThan,
    isLowerThan,

    -- ** Assertion util functions
    focus,
    inside,
    tag,

    -- ** Assertion util functions
    assertThat,

    -- * Types

    -- ** Assertion defitions
    Assertion,
    Assertion',

    -- ** Assertion failure
    FluentTestFailure (..),
  )
where

import Data.Functor.Contravariant (Contravariant (contramap))
import Test.Fluent.Diff (pretty)
import Test.Fluent.Internal.Assertions
  ( Assertion,
    Assertion',
    AssertionDefinition (Assertions),
    FluentTestFailure (..),
    assertThat,
    basicAssertion,
    transformAssertions,
    updateLabel,
  )

-- | The 'simpleAssertion' function is a building block of more complicated assertions.
--
--  It takes one predicate and function to format error message.
--
-- @
--    myIsEqual x = simpleAssertion (== x) (\\x' -> show x' <> " is not equal to " <> show x)
-- @
simpleAssertion ::
  -- | A predicate that should be met by the subject under test
  (a -> Bool) ->
  -- | A function that allows formatting an error message once the predicate is not met
  (a -> String) ->
  Assertion' a a
simpleAssertion predicate formatter f s = basicAssertion predicate formatter (f s)

-- | assert if expected value is equal to given value
-- 
-- @
--    assertThat 15 $ isEqualTo 16
-- @
-- 
-- result
-- 
-- @
--      given 15 should be equal to 16
--       ▼
--      15
--      ╷
--      │
--      ╵
--      16
--       ▲
-- @
isEqualTo :: (Eq a, Show a) => a -> Assertion' a a
isEqualTo a = simpleAssertion (a ==) (formatMessage True "should be equal to" a)

isGreaterThan :: (Ord a, Show a) => a -> Assertion' a a
isGreaterThan a = simpleAssertion (a <) (formatMessage False "should be greater than" a)

isLowerThan :: (Ord a, Show a) => a -> Assertion' a a
isLowerThan a = simpleAssertion (a >) (formatMessage False "should be lower than" a)

formatMessage :: Show a => Bool -> String -> a -> a -> String
formatMessage True message a a' = "given " <> show a' <> " " <> message <> " " <> show a <> "\n" <> pretty a' a
formatMessage False message a a' = "given " <> show a' <> " " <> message <> " " <> show a

focus :: (a -> b) -> Assertion a a b b
focus f assert s = contramap f (assert (f s))

inside :: (b -> a) -> Assertion' a a -> Assertion' b b
inside f assert' b s = Assertions $ b s : transformAssertions [assert' mempty (f s)] f

tag :: String -> Assertion' a a
tag label assert s = updateLabel label (assert s)
