module Test.Fluent.Assertions.List where

import Test.Fluent.Assertions ( Assertion, simpleAssertion )

haveSameSizeAs :: [a] -> Assertion [a]
haveSameSizeAs expected = simpleAssertion (\c -> length c == length expected) (\x -> "sadfasfd")

-- shouldHaveSameElements :: [a] -> Assertion [a]
-- shouldHaveSameElements expected = simpleAssertion a -> Bool a -> String
    -- where 
        -- predicate a = 