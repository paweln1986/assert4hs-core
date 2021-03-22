module TestCase (fluentTestCase) where

import Control.Exception (try)
import Data.Data (Typeable)
import Data.List (intercalate)
import GHC.Exception (SrcLoc (srcLocFile, srcLocStartLine))
import Test.Fluent.Assertions
  ( FluentTestFailure (FluentTestFailure),
  )
import Test.Tasty.Providers
  ( IsTest (..),
    TestName,
    TestTree,
    singleTest,
    testFailedDetails,
    testPassed,
  )
import Test.Tasty.Providers.ConsoleFormat
  ( ResultDetailsPrinter (..),
    failFormat,
  )

newtype FluentTestCase = FluentTestCase (IO String)
  deriving (Typeable)

failedAssertionResultPrinter :: Int -> Int -> ResultDetailsPrinter
failedAssertionResultPrinter errors successes = ResultDetailsPrinter $ \ident formater ->
  formater failFormat (putStrLn $ replicate (ident + 2) ' ' ++ "passed: " ++ show successes ++ ", failed: " ++ show errors ++ ", total: " ++ show (errors + successes))

instance IsTest FluentTestCase where
  run _ (FluentTestCase assertions) _ = do
    result <- try assertions
    pure $
      case result of
        Right info -> testPassed info
        Left (FluentTestFailure _ msg errors successes) -> testFailedDetails (prependLocation msg) (failedAssertionResultPrinter errors successes)
  testOptions = pure []

prependLocation :: [(String, Maybe SrcLoc)] -> String
prependLocation assertionErrors = intercalate "\n\n" $ fmap toLine assertionErrors
  where
    toLine (s, mbloc) = case mbloc of
      Nothing -> s
      Just loc -> "(" <> srcLocFile loc ++ ":" ++ show (srcLocStartLine loc) <> "): \n" <> s

fluentTestCase :: TestName -> IO () -> TestTree
fluentTestCase name = singleTest name . FluentTestCase . fmap (const "")
