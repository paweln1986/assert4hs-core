{-# LANGUAGE TypeApplications #-}

module Lib where

import Control.Exception (Exception, throwIO, try)
import Control.Monad (void)
import Data.Data (Typeable)
import Data.Either
import Data.Function ()
import Data.List
import GHC.Stack (HasCallStack, SrcLoc (srcLocFile, srcLocStartLine), callStack, getCallStack)
import Test.Tasty
import Test.Tasty.Providers (IsTest (..), singleTest, testFailed, testPassed)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype FluentTestCase = FluentTestCase (IO String)
  deriving (Typeable)

newtype AssertionFailure = AssertionFailure {message :: String} deriving (Show)

instance Exception AssertionFailure

data FluentTestFailure = FluentTestFailure
  { srcLoc :: !(Maybe SrcLoc),
    msg :: ![String]
  }
  deriving (Show)

instance Exception FluentTestFailure

data Assertion a
  = Assertions [Assertion a]
  | SimpleAssertion (a -> IO ())

instance Semigroup (Assertion a) where
  Assertions a <> Assertions b = Assertions (a <> b)
  Assertions a <> b = Assertions (a ++ [b])
  b <> Assertions a = Assertions (b : a)

instance Monoid (Assertion a) where
  mempty = Assertions []

simpleAssertion :: HasCallStack => (a -> IO ()) -> Assertion a -> Assertion a
simpleAssertion a b = b <> SimpleAssertion a

assertThat :: HasCallStack => a -> (Assertion a -> Assertion a) -> IO ()
assertThat a b = case b mempty of
  SimpleAssertion e -> do
    res <- try (e a)
    case res of
      Right b -> pure b
      Left (AssertionFailure msg) -> throwIO (FluentTestFailure location [msg])
  assertions -> do
    let extractedAssertions = flattenAssertions assertions
    c <- traverse (\assertion -> try @AssertionFailure $ assertion a) extractedAssertions
    let errors = reverse $ message <$> lefts c
    if null errors then pure () else throwIO (FluentTestFailure location errors)
  where
    location :: Maybe SrcLoc
    location = case reverse (getCallStack callStack) of
      (_, loc) : _ -> Just loc
      [] -> Nothing

flattenAssertions :: Assertion a -> [a -> IO ()]
flattenAssertions (Assertions a) = a >>= flattenAssertions
flattenAssertions (SimpleAssertion assertion) = [assertion]

isEqualTo :: HasCallStack => Eq a => Show a => a -> Assertion a -> Assertion a
isEqualTo a = simpleAssertion (\a' -> if a == a' then pure () else throwIO (AssertionFailure $ "given " <> show a' <> " is not equal to expected " <> show a))

isGreaterThan :: HasCallStack => Ord a => a -> Assertion a -> Assertion a
isGreaterThan a = simpleAssertion (\a' -> (void . pure) (a > a'))

isLowerThan :: HasCallStack => Ord a => a -> Assertion a -> Assertion a
isLowerThan a = simpleAssertion (\a' -> (void . pure) (a > a'))

fluentTestCase :: TestName -> IO () -> TestTree
fluentTestCase name = singleTest name . FluentTestCase . fmap (const "")

instance IsTest FluentTestCase where
  run a (FluentTestCase assertions) _ = do
    result <- try assertions
    pure $
      case result of
        Right info -> testPassed info
        Left (FluentTestFailure srcLoc msg) -> testFailed $ prependLocation srcLoc msg
  testOptions = pure []

prependLocation :: Maybe SrcLoc -> [String] -> String
prependLocation mbloc s =
  case mbloc of
    Nothing -> intercalate "\n\n" s
    Just loc -> srcLocFile loc ++ ":" ++ show (srcLocStartLine loc) ++ ":\n" ++ intercalate "\n" s