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
  ( -- ** Assertion util functions
    assertThrowing,
    assertThrowing',
    assertThrows',
    assertThrows,

    -- ** Exception selectors
    anyException,
    anyIOException,
    exceptionOfType,

    -- ** Exception selector type
    ExceptionSelector,
  )
where

import Control.Exception
  ( Exception (fromException),
    IOException,
    SomeException,
    throwIO,
    try,
  )
import Data.Data (typeOf)
import GHC.Exception
  ( getCallStack,
  )
import GHC.Stack (HasCallStack, callStack)
import Test.Fluent.Assertions (AssertionConfig, defaultConfig, simpleAssertion)
import Test.Fluent.Internal.Assertions
  ( Assertion',
    FluentTestFailure (FluentTestFailure),
    assertThatIO'',
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

assertThrows :: (HasCallStack, Exception e) => IO a -> ExceptionSelector e -> IO ()
assertThrows givenIO selector = assertThrowing' defaultConfig givenIO selector (simpleAssertion (const True) (const "should not be invoked"))

assertThrows' :: (HasCallStack, Exception e) => AssertionConfig -> IO a -> ExceptionSelector e -> IO ()
assertThrows' config givenIO selector = assertThrowing' config givenIO selector (simpleAssertion (const True) (const "should not be invoked"))

assertThrowing :: (HasCallStack, Exception e) => IO a -> ExceptionSelector e -> Assertion' e b -> IO ()
assertThrowing = assertThrowing' defaultConfig

assertThrowing' :: (HasCallStack, Exception e) => AssertionConfig -> IO a -> ExceptionSelector e -> Assertion' e b -> IO ()
assertThrowing' config givenIO predicate = assertThatIO'' config givenIO $ \io -> do
  res <- try io
  case res of
    Left e -> do
      let thrownException = show e
      case fromException e of
        Just expectedException -> pure expectedException
        Nothing -> throwIO (FluentTestFailure location [("should throw an exception of type " <> expectedExceptionName <> " , but " <> thrownException <> " has been thrown", location)] 1 0)
    _ -> throwIO (FluentTestFailure location [("should throw an exception of type " <> expectedExceptionName <> ", but it doesn't", location)] 1 0)
  where
    expectedExceptionName = show $ typeOf (exceptionName predicate)
    exceptionName :: (a -> a) -> a
    exceptionName _ = error "instance of Typeable is broken"
    location = case reverse (getCallStack callStack) of
      (_, loc) : _ -> Just loc
      [] -> Nothing