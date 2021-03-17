module Test.Fluent.Internal.Assertions where

import Control.Exception (Exception, throwIO, try)
import Data.Either (lefts, rights)
import Data.Functor.Contravariant (Contravariant (contramap))
import GHC.Exception (SrcLoc, getCallStack)
import GHC.Stack (HasCallStack, callStack)

data FluentTestFailure = FluentTestFailure
  { srcLoc :: !(Maybe SrcLoc),
    msg :: ![String],
    errorsCount :: !Int,
    successCount :: !Int
  }
  deriving (Show)

instance Exception FluentTestFailure

newtype AssertionFailure = AssertionFailure {message :: String} deriving (Show)

instance Exception AssertionFailure

data AssertionDefinition a
  = Assertions [AssertionDefinition a]
  | SimpleAssertion
      { assertion :: Maybe String -> a -> IO (),
        label :: Maybe String
      }

updateLabel :: String -> AssertionDefinition a -> AssertionDefinition a
updateLabel _ assert@(SimpleAssertion _ (Just _)) = assert
updateLabel assertionLabel (SimpleAssertion a Nothing) = SimpleAssertion a (Just assertionLabel)
updateLabel assertionLabel (Assertions (x : xs)) = Assertions (updateLabel assertionLabel x : fmap (updateLabel assertionLabel) xs)
updateLabel _ (Assertions []) = Assertions []

instance Contravariant AssertionDefinition where
  contramap f (Assertions assertions) = Assertions (fmap (contramap f) assertions)
  contramap f (SimpleAssertion assert assertionLabel) = SimpleAssertion (\l -> assert l . f) assertionLabel

instance Semigroup (AssertionDefinition a) where
  Assertions a <> Assertions b = Assertions (a <> b)
  Assertions a <> b = Assertions (a ++ [b])
  b <> Assertions a = Assertions (b : a)
  a@(SimpleAssertion _ _) <> b@(SimpleAssertion _ _) = Assertions [a, b]

instance Monoid (AssertionDefinition a) where
  mempty = Assertions []

assertThat :: HasCallStack => a -> Assertion' a b -> IO ()
assertThat given b = case b (const mempty) given of
  SimpleAssertion assert assertionLabel -> do
    assertionResult <- try (assert assertionLabel given)
    case assertionResult of
      Right () -> pure ()
      Left (AssertionFailure failureMessage) -> throwIO (FluentTestFailure location [failureMessage] 1 0)
  assertions -> do
    let extractedAssertions = flattenAssertions assertions
    assertionResults <- traverse (\assert -> try $ assert given) extractedAssertions
    let errors = reverse $ message <$> lefts assertionResults
    let successes = length $ rights assertionResults
    if null errors then pure () else throwIO (FluentTestFailure location errors (length errors) successes)
  where
    location :: Maybe SrcLoc
    location = case reverse (getCallStack callStack) of
      (_, loc) : _ -> Just loc
      [] -> Nothing

flattenAssertions :: AssertionDefinition a -> [a -> IO ()]
flattenAssertions (Assertions assertions) = assertions >>= flattenAssertions
flattenAssertions (SimpleAssertion assert assertionLabel) = [assert assertionLabel]

basicAssertion :: (a -> Bool) -> (a -> String) -> AssertionDefinition a -> AssertionDefinition a
basicAssertion predicate messageFormatter b = b <> SimpleAssertion assert Nothing
  where
    assert = \assertionLabel a' ->
      if predicate a'
        then pure ()
        else throwIO (AssertionFailure $ maybe "" (\x -> "[" <> x <> "] ") assertionLabel <> messageFormatter a')

type Assertion s t a b = (a -> AssertionDefinition b) -> s -> AssertionDefinition t

type Assertion' s t = Assertion s s t t