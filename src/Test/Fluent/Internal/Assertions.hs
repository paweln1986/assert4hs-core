{-# OPTIONS_HADDOCK hide, prune, ignore-exports #-}

module Test.Fluent.Internal.Assertions where

import Control.Exception (Exception, throwIO, try)
import Data.Either (isLeft, lefts, rights)
import Data.Functor.Contravariant (Contravariant (contramap))
import GHC.Exception (SrcLoc, getCallStack)
import GHC.Stack (HasCallStack, callStack)

data FluentTestFailure = FluentTestFailure
  { srcLoc :: !(Maybe SrcLoc),
    msg :: ![(String, Maybe SrcLoc)],
    errorsCount :: !Int,
    successCount :: !Int
  }
  deriving (Show)

instance Exception FluentTestFailure

data AssertionFailure = AssertionFailure
  { message :: !String,
    assertionSrcLoc :: !(Maybe SrcLoc)
  }
  deriving (Show)

instance Exception AssertionFailure

data AssertionDefinition a
  = ParallelAssertions [AssertionDefinition a]
  | SequentialAssertions [AssertionDefinition a]
  | SimpleAssertion
      { assertion :: Maybe String -> a -> IO (),
        label :: Maybe String
      }

instance Show (AssertionDefinition a) where
  show (ParallelAssertions a) = "ParallelAssertions " <> show a
  show (SequentialAssertions a) = "SequentialAssertions " <> show a
  show (SimpleAssertion _ assertionLabel) = "SimpleAssertion - " <> show assertionLabel

instance Contravariant AssertionDefinition where
  contramap f (ParallelAssertions assertions) = ParallelAssertions (fmap (contramap f) assertions)
  contramap f (SequentialAssertions assertions) = SequentialAssertions (fmap (contramap f) assertions)
  contramap f (SimpleAssertion assert assertionLabel) = SimpleAssertion (\l -> assert l . f) assertionLabel

instance Semigroup (AssertionDefinition a) where -- TODO: looks like this instance is not lawfull, should be removed
  ParallelAssertions a <> ParallelAssertions b = ParallelAssertions (b <> a)
  ParallelAssertions a <> b@(SequentialAssertions _) = ParallelAssertions (b : a)
  SequentialAssertions a <> b@(ParallelAssertions _) = SequentialAssertions (b : a)
  SequentialAssertions a <> SequentialAssertions b = SequentialAssertions (b <> a)
  s@(SimpleAssertion _ _) <> ParallelAssertions a = ParallelAssertions (a ++ [s])
  ParallelAssertions a <> s@(SimpleAssertion _ _) = ParallelAssertions (s : a)
  s@(SimpleAssertion _ _) <> SequentialAssertions a = SequentialAssertions (a ++ [s])
  SequentialAssertions a <> s@(SimpleAssertion _ _) = SequentialAssertions (s : a)
  a@(SimpleAssertion _ _) <> b@(SimpleAssertion _ _) = ParallelAssertions [b, a]

instance Monoid (AssertionDefinition a) where
  mempty = ParallelAssertions []

updateLabel :: String -> AssertionDefinition a -> AssertionDefinition a
updateLabel newLabel (SimpleAssertion assert (Just oldLabel)) = SimpleAssertion assert (Just $ newLabel <> "." <> oldLabel)
updateLabel assertionLabel (SimpleAssertion a Nothing) = SimpleAssertion a (Just assertionLabel)
updateLabel assertionLabel (ParallelAssertions (x : xs)) = ParallelAssertions (updateLabel assertionLabel x : fmap (updateLabel assertionLabel) xs)
updateLabel assertionLabel (SequentialAssertions (x : xs)) = SequentialAssertions (updateLabel assertionLabel x : fmap (updateLabel assertionLabel) xs)
updateLabel _ (ParallelAssertions []) = ParallelAssertions []
updateLabel _ (SequentialAssertions []) = SequentialAssertions []

assertThat :: HasCallStack => a -> Assertion' a b -> IO ()
assertThat given b = case b (const mempty) given of
  SimpleAssertion assert assertionLabel -> do
    assertionResult <- try (assert assertionLabel given)
    case assertionResult of
      Right () -> pure ()
      Left (AssertionFailure failureMessage assertionLocation) -> throwIO (FluentTestFailure location [(failureMessage, assertionLocation)] 1 0)
  assertions -> do
    assertionResults <- flattenAssertions given assertions
    let errors = (\assertionError -> (message assertionError, assertionSrcLoc assertionError)) <$> lefts assertionResults
    let successes = length $ rights assertionResults
    if null errors then pure () else throwIO (FluentTestFailure location errors (length errors) successes)
  where
    location :: Maybe SrcLoc
    location = case reverse (getCallStack callStack) of
      (_, loc) : _ -> Just loc
      [] -> Nothing

flattenAssertions :: a -> AssertionDefinition a -> IO [Either AssertionFailure ()]
flattenAssertions a (SimpleAssertion assert assertionLabel) = sequence [try $ assert assertionLabel a]
flattenAssertions a (ParallelAssertions assertions) = concat <$> traverse (flattenAssertions a) assertions
flattenAssertions _ (SequentialAssertions []) = pure []
flattenAssertions a (SequentialAssertions (x : xs)) = do
  results <- flattenAssertions a x
  let isFailed = any isLeft results
  if isFailed
    then pure results
    else flattenAssertions a (SequentialAssertions xs)

transformAssertions :: [AssertionDefinition a] -> (b -> a) -> [AssertionDefinition b]
transformAssertions ((SimpleAssertion assert assertionLabel) : xs) f = SimpleAssertion (\l b -> assert (orElse l assertionLabel) (f b)) assertionLabel : transformAssertions xs f
transformAssertions ((ParallelAssertions assertions) : xs) f = ParallelAssertions (transformAssertions assertions f) : transformAssertions xs f
transformAssertions ((SequentialAssertions assertions) : xs) f = SequentialAssertions (transformAssertions assertions f) : transformAssertions xs f
transformAssertions [] _ = []

orElse :: Maybe a -> Maybe a -> Maybe a
x `orElse` y = case x of
  Just _ -> x
  Nothing -> y

basicAssertion :: HasCallStack => (a -> Bool) -> (a -> String) -> AssertionDefinition a -> AssertionDefinition a
basicAssertion predicate messageFormatter b = b <> SimpleAssertion assert Nothing
  where
    assert assertionLabel a' =
      if predicate a'
        then pure ()
        else throwIO (AssertionFailure (maybe "" (\x -> "[" <> x <> "] ") assertionLabel <> messageFormatter a') location)
    location :: Maybe SrcLoc
    location = case reverse (getCallStack callStack) of
      (_, loc) : _ -> Just loc
      [] -> Nothing

basicIOAssertion :: HasCallStack => (a -> Bool) -> (a -> String) -> AssertionDefinition (IO a) -> AssertionDefinition (IO a)
basicIOAssertion predicate messageFormatter b = b <> SimpleAssertion assert Nothing
  where
    assert assertionLabel a' = do
      aaa <- a'
      if predicate aaa
        then pure ()
        else throwIO (AssertionFailure (maybe "" (\x -> "[" <> x <> "] ") assertionLabel <> messageFormatter aaa) location)
    location :: Maybe SrcLoc
    location = case reverse (getCallStack callStack) of
      (_, loc) : _ -> Just loc
      [] -> Nothing

type Assertion'' s t a b = (a -> AssertionDefinition b) -> s -> AssertionDefinition t

type Assertion' a b = Assertion'' a a b b

type Assertion a = Assertion' a a
