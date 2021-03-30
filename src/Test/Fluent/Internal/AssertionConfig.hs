module Test.Fluent.Internal.AssertionConfig where

newtype AssertionConfig = AssertionConfig
  { assertionTimeout :: Int
  }
  deriving (Show)

-- | Default configuration used for `assertThat` and `assertThatIO`.
-- - default timeout is set to 5 seconds
defaultConfig :: AssertionConfig
defaultConfig =
  AssertionConfig
    5000000 -- 5 seconds

-- | Allow to modify timeout of single assertion
setAssertionTimeout :: Int -> AssertionConfig -> AssertionConfig
setAssertionTimeout newTimeout config = config {assertionTimeout = newTimeout}