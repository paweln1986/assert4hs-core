{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_HADDOCK hide, prune, ignore-exports #-}

module Test.Fluent.Internal.Assertions where

import Control.Exception (Exception, throwIO, try)
import Data.Either (isLeft, lefts, rights)
import Data.Functor.Contravariant (Contravariant (contramap))
import GHC.Exception (SrcLoc, getCallStack)
import GHC.Stack (HasCallStack, callStack)
import System.Timeout (timeout)
import Test.Fluent.Internal.AssertionConfig
  ( AssertionConfig (assertionTimeout),
    defaultConfig,
  )

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

instance Semigroup (AssertionDefinition a) where -- TODO: looks like this instance is not lawful, should be removed and replaced by plain function
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

-- | Execute assertions against given subject under test.
assertThat :: HasCallStack => a -> Assertion' a b -> IO ()
assertThat given = assertThatIO (pure given)

-- | Execute assertions against given subject under test extracted from IO action.
assertThatIO :: HasCallStack => IO a -> Assertion' a b -> IO ()
assertThatIO given = assertThatIO'' defaultConfig given id

-- | A variant of `assertThat` which allow to pass additional configuration.
assertThat' :: HasCallStack => AssertionConfig -> a -> Assertion' a b -> IO ()
assertThat' config given = assertThatIO' config (pure given)

-- | A variant of `assertThatIO` which allow to pass additional configuration.
assertThatIO' :: HasCallStack => AssertionConfig -> IO a -> Assertion' a c -> IO ()
assertThatIO' config givenIO = assertThatIO'' config givenIO id

assertThatIO'' :: HasCallStack => AssertionConfig -> IO a -> (IO a -> IO b) -> Assertion' b c -> IO ()
assertThatIO'' config givenIO f b = do
  given <- f givenIO
  case b (const mempty) given of
    SimpleAssertion assert assertionLabel -> do
      assertionResult <- try (assert assertionLabel given)
      case assertionResult of
        Right () -> pure ()
        Left (AssertionFailure failureMessage assertionLocation) -> throwIO (FluentTestFailure location [(failureMessage, assertionLocation)] 1 0)
    assertions -> do
      assertionResults <- flattenAssertions config given assertions
      let errors = (\assertionError -> (message assertionError, assertionSrcLoc assertionError)) <$> lefts assertionResults
      let successes = length $ rights assertionResults
      if null errors then pure () else throwIO (FluentTestFailure location errors (length errors) successes)
  where
    location = case reverse (getCallStack callStack) of
      (_, loc) : _ -> Just loc
      [] -> Nothing

flattenAssertions :: HasCallStack => AssertionConfig -> a -> AssertionDefinition a -> IO [Either AssertionFailure ()]
flattenAssertions config a (SimpleAssertion assert assertionLabel) = sequence [executeAssertion config assert assertionLabel a]
flattenAssertions config a (ParallelAssertions assertions) = concat <$> traverse (flattenAssertions config a) assertions
flattenAssertions _ _ (SequentialAssertions []) = pure []
flattenAssertions config a (SequentialAssertions (x : xs)) = do
  results <- flattenAssertions config a x
  let isFailed = any isLeft results
  if isFailed
    then pure results
    else flattenAssertions config a (SequentialAssertions xs)

executeAssertion :: HasCallStack => AssertionConfig -> (Maybe String -> t2 -> IO ()) -> Maybe String -> t2 -> IO (Either AssertionFailure ())
executeAssertion config assert assertionLabel given = do
  result <- withTimeout $ do
    !assertionResult <- try $ assert assertionLabel given
    return assertionResult
  case result of
    Nothing ->
      pure (Left $ AssertionFailure (maybe "" (\x -> "[" <> x <> "] ") assertionLabel <> timeoutMessage) location)
    Just a -> pure a
  where
    withTimeout = timeout $ assertionTimeout config
    timeoutMessage = "Timeout occurred, probably some infinitive data structure or not terminating predicate has been used. Timeout: " <> show timeoutInSeconds <> "s"
    timeoutInSeconds :: Double
    timeoutInSeconds = fromIntegral (assertionTimeout config) / 1000000.0
    location :: Maybe SrcLoc
    location = case reverse (getCallStack callStack) of
      (_, loc) : _ -> Just loc
      [] -> Nothing

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
