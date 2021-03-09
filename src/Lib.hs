{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Lib where

import Control.Exception (Exception, throwIO, try)
import Control.Monad (void)
import Data.Data (Typeable)
import Data.Either (lefts, rights)
import Data.Function ()
import Data.Functor.Contravariant (Contravariant (contramap))
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import GHC.Stack (HasCallStack, SrcLoc (srcLocFile, srcLocStartLine), callStack, getCallStack)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Providers (IsTest (..), singleTest, testFailed, testFailedDetails, testPassed)
import Test.Tasty.Providers.ConsoleFormat (ConsoleFormatPrinter, ResultDetailsPrinter (..), failFormat, infoFailFormat, noResultDetails)

newtype FluentTestCase = FluentTestCase (IO String)
  deriving (Typeable)

newtype AssertionFailure = AssertionFailure {message :: String} deriving (Show)

instance Exception AssertionFailure

data FluentTestFailure = FluentTestFailure
  { srcLoc :: !(Maybe SrcLoc),
    msg :: ![String],
    errorsCount :: !Int,
    successCount :: !Int
  }
  deriving (Show)

instance Exception FluentTestFailure

data Assertion a
  = Assertions [Assertion a]
  | SimpleAssertion
      { assertion :: Maybe String -> a -> IO (),
        label :: Maybe String
      }

updateLabel :: String -> Assertion a -> Assertion a
updateLabel label assert@(SimpleAssertion a (Just l)) = assert
updateLabel label (SimpleAssertion a Nothing) = SimpleAssertion a (Just label)
updateLabel label (Assertions (x : xs)) = Assertions (updateLabel label x : fmap (updateLabel label) xs)
updateLabel label (Assertions []) = Assertions []

instance Contravariant Assertion where
  contramap f (Assertions assertions) = Assertions (fmap (contramap f) assertions)
  contramap f (SimpleAssertion assert label) = SimpleAssertion (\l -> assert l . f) label

instance Semigroup (Assertion a) where
  Assertions a <> Assertions b = Assertions (a <> b)
  Assertions a <> b = Assertions (a ++ [b])
  b <> Assertions a = Assertions (b : a)

instance Monoid (Assertion a) where
  mempty = Assertions []

assertThat :: HasCallStack => a -> Asser' a b -> IO ()
assertThat given b = case b (const mempty) given of
  SimpleAssertion assertion label -> do
    assertionResult <- try (assertion label given)
    case assertionResult of
      Right () -> pure ()
      Left (AssertionFailure msg) -> throwIO (FluentTestFailure location [msg] 1 0)
  assertions -> do
    let extractedAssertions = flattenAssertions assertions
    assertionResults <- traverse (\assertion -> try $ assertion given) extractedAssertions
    let errors = reverse $ message <$> lefts assertionResults
    let successes = length $ rights assertionResults
    if null errors then pure () else throwIO (FluentTestFailure location errors (length errors) successes)
  where
    location :: Maybe SrcLoc
    location = case reverse (getCallStack callStack) of
      (_, loc) : _ -> Just loc
      [] -> Nothing

flattenAssertions :: Assertion a -> [a -> IO ()]
flattenAssertions (Assertions assertions) = assertions >>= flattenAssertions
flattenAssertions (SimpleAssertion assertion label) = [assertion label]

basicAssertion :: HasCallStack => (a -> Bool) -> (a -> String) -> Assertion a -> Assertion a
basicAssertion predicate messageFormatter b = b <> SimpleAssertion assertion Nothing
  where
    assertion = \label a' -> if predicate a' then pure () else throwIO (AssertionFailure $ maybe "" (\x -> "[" <> x <> "] ") label <> messageFormatter a')

fluentTestCase :: TestName -> IO () -> TestTree
fluentTestCase name = singleTest name . FluentTestCase . fmap (const "")

failedAssertionResultPrinter :: Int -> Int -> ResultDetailsPrinter
failedAssertionResultPrinter errors successes = ResultDetailsPrinter $ \ident formater ->
  formater failFormat (putStrLn $ replicate (ident + 2) ' ' ++ "passed: " ++ show successes ++ ", failed: " ++ show errors ++ ", total: " ++ show (errors + successes))

instance IsTest FluentTestCase where
  run _ (FluentTestCase assertions) _ = do
    result <- try assertions
    pure $
      case result of
        Right info -> testPassed info
        Left (FluentTestFailure srcLoc msg errors successes) -> testFailedDetails (prependLocation srcLoc msg) (failedAssertionResultPrinter errors successes)
  testOptions = pure []

prependLocation :: Maybe SrcLoc -> [String] -> String
prependLocation mbloc s =
  case mbloc of
    Nothing -> intercalate "\n\n" s
    Just loc -> srcLocFile loc ++ ":" ++ show (srcLocStartLine loc) ++ ":\n" ++ intercalate "\n" s

type Asser s t a b = (a -> Assertion b) -> s -> Assertion t

type Asser' s t = Asser s s t t

simpleAssertion :: (a -> Bool) -> (a -> String) -> Asser' a a
simpleAssertion predicate formatter f s = basicAssertion predicate formatter (f s)

isEqualTo :: (Eq a, Show a) => a -> Asser' a a
isEqualTo a = simpleAssertion (a ==) (\a' -> "expected " <> show a' <> " is not equal to given " <> show a)

isGreaterThan :: (Ord a, Show a) => a -> Asser' a a
isGreaterThan a = simpleAssertion (a <) (\a' -> "expected " <> show a' <> " to be greater than " <> show a)

isLowerThan :: (Ord a, Show a) => a -> Asser' a a
isLowerThan a = simpleAssertion (a >) (\a' -> "expected " <> show a' <> " to be lower than " <> show a)

focus :: (a -> b) -> Asser a a b b
focus f assert s = contramap f (assert (f s))

tag :: String -> Asser a a a a
tag label assert s = updateLabel label (assert s)