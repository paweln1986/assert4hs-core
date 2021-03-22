{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      : Test.Fluent.Assertions.Maybe
-- Description : Set of assertions for Maybe type
-- Copyright   : (c) Pawel Nosal, 2021
-- License     : MIT
-- Maintainer  : p.nosal1986@gmail.com
-- Stability   : experimental
--
-- This library aims to provide a set of combinators to assert Maybe type.
module Test.Fluent.Assertions.Maybe (isNothing, isJust, extracting) where

import qualified Data.Maybe as Maybe
import GHC.Stack (HasCallStack)
import Test.Fluent.Assertions
  ( focus,
    forceError,
    inside,
    simpleAssertion,
  )
import Test.Fluent.Internal.Assertions (Assertion', Assertion)

-- | assert if subject under is empty
--
-- @
--  assertThat (Just 10) isNothing
-- @
isNothing :: HasCallStack => Assertion (Maybe a)
isNothing = inside Maybe.isNothing (simpleAssertion (== True) assertionMessage)
  where
    assertionMessage _ = "should be Nothing"

-- | assert if subject under is not empty
--
-- @
--  assertThat (Just 10) isJust
-- @
isJust :: HasCallStack => Assertion (Maybe a)
isJust = inside Maybe.isJust (simpleAssertion (== True) assertionMessage)
  where
    assertionMessage _ = "should be Just"

-- | assert if subject under is not empty and extract contained value
--
-- @
--  assertThat (Just 10) extracting
-- @
extracting :: HasCallStack => Assertion' (Maybe a) a
extracting = forceError isJust . focus Maybe.fromJust
