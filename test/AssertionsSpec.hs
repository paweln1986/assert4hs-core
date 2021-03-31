{-# LANGUAGE ScopedTypeVariables #-}

module AssertionsSpec where

import Control.Exception (try)
import Data.Bifunctor (Bifunctor (first, second))
import Data.Either (fromLeft, isRight)
import Data.Maybe ()
import GHC.Stack
  ( HasCallStack,
    SrcLoc
      ( SrcLoc,
        srcLocEndLine,
        srcLocStartLine
      ),
    callStack,
    getCallStack,
  )
import Test.Fluent.Assertions
  ( contains,
    hasSize,
    isEmpty,
    isEqualTo,
    isGreaterEqualThan,
    isGreaterThan,
    isLowerEqualThan,
    isLowerThan,
    isNotEmpty,
    isNotEqualTo,
    shouldSatisfy,
  )
import Test.Fluent.Internal.Assertions
  ( FluentTestFailure (FluentTestFailure, msg, srcLoc),
    assertThat,
  )
import Test.Hspec (Spec, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "failing assertions" $ do
    it "isEqualTo should report correct message" $ do
      ((startLine, endLine), res) <-
        testLocation 1 $
          assertThat "someString" $
            isEqualTo "otherString"
      isRight res `shouldBe` False
      let (FluentTestFailure assertThatLoc messages erros success) = fromLeft undefined res
      erros `shouldBe` 1
      success `shouldBe` 0
      assertThatLoc `shouldBe` Just (SrcLoc "main" "AssertionsSpec" "test/AssertionsSpec.hs" startLine 0 endLine 0)
      messages
        `shouldBe` [ ( "given \"someString\" should be equal to \"otherString\"\n \9660 \9660\n\"someString\"\n\9591\n\9474\n\9589\n\"otherString\"\n  \9650\9650 \9650\n",
                       Just (SrcLoc "main" "AssertionsSpec" "test/AssertionsSpec.hs" (startLine + 1) 0 (endLine + 1) 0)
                     )
                   ]
    it "isNotEqualTo should report correct message" $ do
      ((startLine, endLine), res) <-
        testLocation 1 $
          assertThat "someString" $
            isNotEqualTo "someString"
      isRight res `shouldBe` False
      let (FluentTestFailure assertThatLoc messages erros success) = fromLeft undefined res
      erros `shouldBe` 1
      success `shouldBe` 0
      assertThatLoc `shouldBe` Just (SrcLoc "main" "AssertionsSpec" "test/AssertionsSpec.hs" startLine 0 endLine 0)
      messages
        `shouldBe` [ ( "given \"someString\" should be not equal to \"someString\"",
                       Just (SrcLoc "main" "AssertionsSpec" "test/AssertionsSpec.hs" (startLine + 1) 0 (endLine + 1) 0)
                     )
                   ]
    it "isGreaterThan should report correct message" $ do
      ((startLine, endLine), res) <- testLocation 0 $ assertThat 5 $ isGreaterThan 6
      isRight res `shouldBe` False
      let (FluentTestFailure assertThatLoc messages erros success) = fromLeft undefined res
      erros `shouldBe` 1
      success `shouldBe` 0
      assertThatLoc `shouldBe` Just (SrcLoc "main" "AssertionsSpec" "test/AssertionsSpec.hs" startLine 0 endLine 0)
      messages
        `shouldBe` [ ( "given 5 should be greater than 6",
                       Just (SrcLoc "main" "AssertionsSpec" "test/AssertionsSpec.hs" startLine 0 endLine 0)
                     )
                   ]

    it "isGreaterEqualThan should report correct message" $ do
      ((startLine, endLine), res) <- testLocation 0 $ assertThat 5 $ isGreaterEqualThan 6
      isRight res `shouldBe` False
      let (FluentTestFailure assertThatLoc messages erros success) = fromLeft undefined res
      erros `shouldBe` 1
      success `shouldBe` 0
      assertThatLoc `shouldBe` Just (SrcLoc "main" "AssertionsSpec" "test/AssertionsSpec.hs" startLine 0 endLine 0)
      messages
        `shouldBe` [ ( "given 5 should be greater or equal to 6",
                       Just (SrcLoc "main" "AssertionsSpec" "test/AssertionsSpec.hs" startLine 0 endLine 0)
                     )
                   ]

    it "isLowerThan should report correct message" $ do
      ((startLine, endLine), res) <- testLocation 0 $ assertThat 5 $ isLowerThan 4
      isRight res `shouldBe` False
      let (FluentTestFailure assertThatLoc messages erros success) = fromLeft undefined res
      erros `shouldBe` 1
      success `shouldBe` 0
      assertThatLoc `shouldBe` Just (SrcLoc "main" "AssertionsSpec" "test/AssertionsSpec.hs" startLine 0 endLine 0)
      messages
        `shouldBe` [ ( "given 5 should be lower than 4",
                       Just (SrcLoc "main" "AssertionsSpec" "test/AssertionsSpec.hs" startLine 0 endLine 0)
                     )
                   ]
    it "isLowerEqualThan should report correct message" $ do
      ((startLine, endLine), res) <- testLocation 0 $ assertThat 5 $ isLowerEqualThan 4
      isRight res `shouldBe` False
      let (FluentTestFailure assertThatLoc messages erros success) = fromLeft undefined res
      erros `shouldBe` 1
      success `shouldBe` 0
      assertThatLoc `shouldBe` Just (SrcLoc "main" "AssertionsSpec" "test/AssertionsSpec.hs" startLine 0 endLine 0)
      messages
        `shouldBe` [ ( "given 5 should be lower or equal to 4",
                       Just (SrcLoc "main" "AssertionsSpec" "test/AssertionsSpec.hs" startLine 0 endLine 0)
                     )
                   ]
    it "shouldSatisfy should report correct message" $ do
      ((startLine, endLine), res) <- testLocation 0 $ assertThat 5 $ shouldSatisfy (< 5)
      isRight res `shouldBe` False
      let (FluentTestFailure assertThatLoc messages erros success) = fromLeft undefined res
      erros `shouldBe` 1
      success `shouldBe` 0
      assertThatLoc `shouldBe` Just (SrcLoc "main" "AssertionsSpec" "test/AssertionsSpec.hs" startLine 0 endLine 0)
      messages
        `shouldBe` [ ( "given 5 does not met a predicate",
                       Just (SrcLoc "main" "AssertionsSpec" "test/AssertionsSpec.hs" startLine 0 endLine 0)
                     )
                   ]
    it "hasSize should report correct message" $ do
      ((startLine, endLine), res) <- testLocation 0 $ assertThat [1 .. 7] $ hasSize 5
      isRight res `shouldBe` False
      let (FluentTestFailure assertThatLoc messages erros success) = fromLeft undefined res
      erros `shouldBe` 1
      success `shouldBe` 0
      assertThatLoc `shouldBe` Just (SrcLoc "main" "AssertionsSpec" "test/AssertionsSpec.hs" startLine 0 endLine 0)
      messages
        `shouldBe` [ ( "expected size 5 is not equal actual size 7",
                       Just (SrcLoc "main" "AssertionsSpec" "test/AssertionsSpec.hs" startLine 0 endLine 0)
                     )
                   ]
    it "isEmpty should report correct message" $ do
      ((startLine, endLine), res) <- testLocation 0 $ assertThat (Just 1) $ isEmpty
      isRight res `shouldBe` False
      let (FluentTestFailure assertThatLoc messages erros success) = fromLeft undefined res
      erros `shouldBe` 1
      success `shouldBe` 0
      assertThatLoc `shouldBe` Just (SrcLoc "main" "AssertionsSpec" "test/AssertionsSpec.hs" startLine 0 endLine 0)
      messages
        `shouldBe` [ ( "should be empty, but is not",
                       Just (SrcLoc "main" "AssertionsSpec" "test/AssertionsSpec.hs" startLine 0 endLine 0)
                     )
                   ]
    it "isNotEmpty should report correct message" $ do
      ((startLine, endLine), res) <- testLocation 0 $ assertThat Nothing $ isNotEmpty
      isRight res `shouldBe` False
      let (FluentTestFailure assertThatLoc messages erros success) = fromLeft undefined res
      erros `shouldBe` 1
      success `shouldBe` 0
      assertThatLoc `shouldBe` Just (SrcLoc "main" "AssertionsSpec" "test/AssertionsSpec.hs" startLine 0 endLine 0)
      messages
        `shouldBe` [ ( "should be not empty",
                       Just (SrcLoc "main" "AssertionsSpec" "test/AssertionsSpec.hs" startLine 0 endLine 0)
                     )
                   ]
    it "contains should report correct message" $ do
      ((startLine, endLine), res) <- testLocation 0 $ assertThat (Just 1) $ contains 10
      isRight res `shouldBe` False
      let (FluentTestFailure assertThatLoc messages erros success) = fromLeft undefined res
      erros `shouldBe` 1
      success `shouldBe` 0
      assertThatLoc `shouldBe` Just (SrcLoc "main" "AssertionsSpec" "test/AssertionsSpec.hs" startLine 0 endLine 0)
      messages
        `shouldBe` [ ( "should contain element 10, but it doesn't",
                       Just (SrcLoc "main" "AssertionsSpec" "test/AssertionsSpec.hs" startLine 0 endLine 0)
                     )
                   ]
  describe "success assertions" $ do
    it "isEqualTo should report correct message" $ do
      (_, res) <-
        testLocation 1 $
          assertThat "someString" $
            isEqualTo "someString"
      isRight res `shouldBe` True
    it "isNotEqualTo should report correct message" $ do
      (_, res) <-
        testLocation 1 $
          assertThat "someString" $
            isNotEqualTo "otherString"
      isRight res `shouldBe` True
    it "isGreaterThan should report correct message" $ do
      (_, res) <- testLocation 0 $ assertThat 5 $ isGreaterThan 4
      isRight res `shouldBe` True
    it "isGreaterEqualThan should report correct message" $ do
      (_, res) <- testLocation 0 $ assertThat 5 $ isGreaterEqualThan 5
      isRight res `shouldBe` True
    it "isLowerThan should report correct message" $ do
      (_, res) <- testLocation 0 $ assertThat 5 $ isLowerThan 6
      isRight res `shouldBe` True
    it "isLowerEqualThan should report correct message" $ do
      (_, res) <- testLocation 0 $ assertThat 5 $ isLowerEqualThan 5
      isRight res `shouldBe` True
    it "shouldSatisfy should report correct message" $ do
      (_, res) <- testLocation 0 $ assertThat 5 $ shouldSatisfy (> 4)
      isRight res `shouldBe` True
    it "hasSize should report correct message" $ do
      (_, res) <- testLocation 0 $ assertThat [0 .. 4] $ hasSize 5
      isRight res `shouldBe` True
    it "isEmpty should report correct message" $ do
      (_, res) <- testLocation 0 $ assertThat Nothing $ isEmpty
      isRight res `shouldBe` True
    it "isNotEmpty should report correct message" $ do
      (_, res) <- testLocation 0 $ assertThat (Just 1) $ isNotEmpty
      isRight res `shouldBe` True
    it "contains should report correct message" $ do
      (_, res) <- testLocation 0 $ assertThat (Just 10) $ contains 10
      isRight res `shouldBe` True

testLocation :: HasCallStack => Int -> IO () -> IO ((Int, Int), Either FluentTestFailure ())
testLocation offsetLine assertionToTest = do
  res <- try assertionToTest
  let res1 = first (\f -> f {srcLoc = aaa (srcLoc f), msg = bbb (msg f)}) res
  pure ((srcLocStartLine loc + offsetLine, srcLocEndLine loc + offsetLine), res1)
  where
    loc = snd $ head $ getCallStack callStack
    aaa (Just (SrcLoc a b c lineStart _ lineEnd _)) = Just (SrcLoc a b c lineStart 0 lineEnd 0)
    aaa Nothing = Nothing
    bbb x = fmap (second aaa) x