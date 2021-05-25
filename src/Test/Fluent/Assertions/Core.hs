{-# OPTIONS_HADDOCK show-extensions #-}

module Test.Fluent.Assertions.Core
  ( -- ** Assertion util functions
    assertThat,
    assertThatIO,
    assertThat',
    assertThatIO',
    assertThrown,
    assertThrown',
    assertThrows',
    assertThrows,
  )
where

import Control.Exception (Exception (fromException), throwIO, try)
import Data.Data (typeOf)
import GHC.Stack (HasCallStack, callStack, getCallStack)
import Test.Fluent.Assertions (FluentTestFailure (..), simpleAssertion)
import Test.Fluent.Assertions.Exceptions (ExceptionSelector)
import Test.Fluent.Internal.AssertionConfig
  ( AssertionConfig,
    defaultConfig,
  )
import Test.Fluent.Internal.Assertions
  ( Assertion',
    assertThat,
    assertThat',
    assertThatIO,
    assertThatIO',
    assertThatIO'',
  )

-- |
-- Module      : Test.Fluent.Assertions.Core
-- Description : Set util function to execute assertions against given value
-- Copyright   : (c) Pawel Nosal, 2021
-- License     : MIT
-- Maintainer  : p.nosal1986@gmail.com
-- Stability   : experimental

-- | Verify if given `IO` action throws the expected exception.
assertThrows :: (HasCallStack, Exception e) => IO a -> ExceptionSelector e -> IO ()
assertThrows givenIO selector = assertThrown' defaultConfig givenIO selector (simpleAssertion (const True) (const "should not be invoked"))

assertThrows' :: (HasCallStack, Exception e) => AssertionConfig -> IO a -> ExceptionSelector e -> IO ()
assertThrows' config givenIO selector = assertThrown' config givenIO selector (simpleAssertion (const True) (const "should not be invoked"))

-- | Execute assertions against selected exception
assertThrown :: (HasCallStack, Exception e) => IO a -> ExceptionSelector e -> Assertion' e b -> IO ()
assertThrown = assertThrown' defaultConfig

assertThrown' :: (HasCallStack, Exception e) => AssertionConfig -> IO a -> ExceptionSelector e -> Assertion' e b -> IO ()
assertThrown' config givenIO predicate = assertThatIO'' config givenIO $ \io -> do
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
