module Test.Fluent.Assertions.List where

import Test.Fluent.Assertions ( Assertion, simpleAssertion )

haveSameSizeAs :: [a] -> Assertion [a]
haveSameSizeAs expected = simpleAssertion (\c -> length c == length expected) (\x -> "should have same leght, but they don't. Given: " <> show (length x) <> " is not equal " <> show (length expected))

-- shouldHaveSameElements :: [a] -> Assertion [a]
-- shouldHaveSameElements expected = simpleAssertion a -> Bool a -> String
    -- where 
        -- predicate a = 