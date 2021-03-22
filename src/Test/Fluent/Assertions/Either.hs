{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      : Test.Fluent.Assertions.Either
-- Description : Set of assertions for Either type
-- Copyright   : (c) Pawel Nosal, 2021
-- License     : MIT
-- Maintainer  : p.nosal1986@gmail.com
-- Stability   : experimental
--
-- This library aims to provide a set of combinators to assert Either type.
module Test.Fluent.Assertions.Either (isLeft, isRight, extractingRight, extractingLeft) where

import qualified Data.Either as Either
import GHC.Stack (HasCallStack)
import Test.Fluent.Assertions
  ( Assertion,
    Assertion',
    focus,
    forceError,
    inside,
    simpleAssertion,
  )

-- | assert if subject under is Left
--
-- @
--  assertThat (Left 10) isLeft
-- @
isLeft :: HasCallStack => Assertion (Either a b)
isLeft = inside Either.isLeft (simpleAssertion (== True) assertionMessage)
  where
    assertionMessage _ = "should be Left, but is Right"

-- | assert if subject under is Left
--
-- @
--  assertThat (Left 10) isRight
-- @
isRight :: HasCallStack => Assertion (Either a b)
isRight = inside Either.isRight (simpleAssertion (== True) assertionMessage)
  where
    assertionMessage _ = "should be Right, but is Left"

-- | assert if subject under test is Right and extract contained value
--
-- @
--  assertThat (Left 10) extractingRight
-- @
extractingRight :: HasCallStack => Assertion' (Either a b) b
extractingRight = forceError isRight . focus (\case (Right a) -> a)

-- | assert if subject under test is Left and extract contained value
--
-- @
--  assertThat (Left 10) extractingLeft
-- @
extractingLeft :: HasCallStack => Assertion' (Either a b) a
extractingLeft = forceError isLeft . focus (\case (Left a) -> a)
