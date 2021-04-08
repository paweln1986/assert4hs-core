{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      : Test.Fluent.Assertions.Exceptions
-- Description : Set of assertions for Exception type
-- Copyright   : (c) Pawel Nosal, 2021
-- License     : MIT
-- Maintainer  : p.nosal1986@gmail.com
-- Stability   : experimental
--
-- This module provide an assertion for check if expected Exception has been throw by IO action.
module Test.Fluent.Assertions.Exceptions
  ( -- ** Exception selectors
    anyException,
    anyIOException,
    exceptionOfType,

    -- ** Exception selector type
    ExceptionSelector,
  )
where

import Control.Exception
  ( Exception,
    IOException,
    SomeException,
  )

type ExceptionSelector a = a -> a

-- | Select all exceptions.
anyException :: ExceptionSelector SomeException
anyException = id

-- | Select all IOException.
anyIOException :: ExceptionSelector IOException
anyIOException = id

-- | Select all an Exception of given type.
-- This selector should be used with `TypeApplications`
--
-- @
-- data MyException = ThisException | ThatException
--  deriving (Show)
--
-- instance Exception MyException
--
-- selectMyException = exceptionType @MyException
-- @
exceptionOfType :: Exception e => ExceptionSelector e
exceptionOfType = id
