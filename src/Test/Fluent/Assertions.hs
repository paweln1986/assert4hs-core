module Test.Fluent.Assertions (Assertion, Assertion', simpleAssertion, FluentTestFailure (..), assertThat, isEqualTo, isGreaterThan, isLowerThan, focus, tag) where

import Data.Functor.Contravariant (Contravariant (contramap))
import Test.Fluent.Diff (pretty)
import Test.Fluent.Internal.Assertions
  ( Assertion,
    Assertion',
    FluentTestFailure (..),
    assertThat,
    basicAssertion,
    updateLabel,
  )

simpleAssertion :: (a -> Bool) -> (a -> String) -> Assertion' a a
simpleAssertion predicate formatter f s = basicAssertion predicate formatter (f s)

isEqualTo :: (Eq a, Show a) => a -> Assertion' a a
isEqualTo a = simpleAssertion (a ==) (formatMessage True "should be equal to" a)

isGreaterThan :: (Ord a, Show a) => a -> Assertion' a a
isGreaterThan a = simpleAssertion (a <) (formatMessage False "should be greater than" a)

isLowerThan :: (Ord a, Show a) => a -> Assertion' a a
isLowerThan a = simpleAssertion (a >) (formatMessage False "should be lower than" a)

formatMessage :: Show a => Bool -> String -> a -> a -> String
formatMessage True message a a' = "given " <> show a' <> " " <> message <> " " <> show a <> "\n" <> pretty a' a
formatMessage False message a a' = "given " <> show a' <> " " <> message <> " " <> show a

focus :: (a -> b) -> Assertion a a b b
focus f assert s = contramap f (assert (f s))

tag :: String -> Assertion a a a a
tag label assert s = updateLabel label (assert s)
