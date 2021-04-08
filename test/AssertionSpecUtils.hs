module AssertionSpecUtils where

import Control.Exception (try)
import Data.Bifunctor (Bifunctor (second), first)
import Data.Maybe (listToMaybe)
import GHC.Exception
  ( SrcLoc (SrcLoc, srcLocEndLine, srcLocStartLine),
    getCallStack,
  )
import GHC.Stack (HasCallStack, callStack)
import Test.Fluent.Assertions (FluentTestFailure (msg, srcLoc))

assertionMessage :: HasCallStack => String -> Int -> Int -> (String, Maybe SrcLoc)
assertionMessage message startLine endLine = (message, updateLocation loc)
  where
    loc = snd <$> listToMaybe (getCallStack callStack)
    updateLocation (Just (SrcLoc a b c _ _ _ _)) = Just (SrcLoc a b c startLine 0 endLine 0)
    updateLocation Nothing = Nothing

testLocation :: HasCallStack => Int -> IO () -> IO ((Int, Int), Either FluentTestFailure ())
testLocation offsetLine assertionToTest = do
  res <- try assertionToTest
  let updatedRes = first (\f -> f {srcLoc = updateLocation (srcLoc f), msg = updateAssertionLocation (msg f)}) res
  pure ((srcLocStartLine loc + offsetLine, srcLocEndLine loc + offsetLine), updatedRes)
  where
    loc = snd $ head $ getCallStack callStack
    updateLocation (Just (SrcLoc a b c lineStart _ lineEnd _)) = Just (SrcLoc a b c lineStart 0 lineEnd 0)
    updateLocation Nothing = Nothing
    updateAssertionLocation x = fmap (second updateLocation) x
