{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module AssertionsSpec where

import AssertionSpecUtils (assertionMessage, testLocation)
import Data.Either (fromLeft, isRight)
import Data.Function ((&))
import Data.Maybe ()
import GHC.Stack
  ( SrcLoc
      ( SrcLoc
      ),
  )
import Test.Fluent.Assertions
  ( contains,
    defaultConfig,
    hasSize,
    isEmpty,
    isEqualTo,
    isGreaterEqualThan,
    isGreaterThan,
    isLowerEqualThan,
    isLowerThan,
    isNotEmpty,
    isNotEqualTo,
    setAssertionTimeout,
    shouldSatisfy,
  )
import Test.Fluent.Internal.Assertions
  ( FluentTestFailure (FluentTestFailure),
    assertThat,
    assertThat',
  )
import Test.Hspec (Spec, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "chaining assertions" $ do
    it "should accumulate errors" $ do
      ((startLine, endLine), res) <-
        testLocation 1 $
          assertThat "someString" $
            isLowerThan "someString"
              . isNotEqualTo "someString"
              . isGreaterThan "someString"
      isRight res `shouldBe` False
      let (FluentTestFailure assertThatLoc messages erros success) = fromLeft undefined res
      erros `shouldBe` 3
      success `shouldBe` 0
      assertThatLoc `shouldBe` Just (SrcLoc "main" "AssertionsSpec" "test/AssertionsSpec.hs" startLine 0 endLine 0)
      messages
        `shouldBe` [ assertionMessage "given \"someString\" should be lower than \"someString\"" (startLine + 1) (endLine + 1),
                     assertionMessage "given \"someString\" should be not equal to \"someString\"" (startLine + 2) (endLine + 2),
                     assertionMessage "given \"someString\" should be greater than \"someString\"" (startLine + 3) (endLine + 3)
                   ]
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
        `shouldBe` [assertionMessage "given \"someString\" should be equal to \"otherString\"\n \9660 \9660\n\"someString\"\n\9591\n\9474\n\9589\n\"otherString\"\n  \9650\9650 \9650\n" (startLine + 1) (endLine + 1)]
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
        `shouldBe` [assertionMessage "given \"someString\" should be not equal to \"someString\"" (startLine + 1) (endLine + 1)]
    it "isGreaterThan should report correct message" $ do
      ((startLine, endLine), res) <- testLocation 0 $ assertThat 5 $ isGreaterThan 6
      isRight res `shouldBe` False
      let (FluentTestFailure assertThatLoc messages erros success) = fromLeft undefined res
      erros `shouldBe` 1
      success `shouldBe` 0
      assertThatLoc `shouldBe` Just (SrcLoc "main" "AssertionsSpec" "test/AssertionsSpec.hs" startLine 0 endLine 0)
      messages
        `shouldBe` [assertionMessage "given 5 should be greater than 6" startLine endLine]
    it "isGreaterEqualThan should report correct message" $ do
      ((startLine, endLine), res) <- testLocation 0 $ assertThat 5 $ isGreaterEqualThan 6
      isRight res `shouldBe` False
      let (FluentTestFailure assertThatLoc messages erros success) = fromLeft undefined res
      erros `shouldBe` 1
      success `shouldBe` 0
      assertThatLoc `shouldBe` Just (SrcLoc "main" "AssertionsSpec" "test/AssertionsSpec.hs" startLine 0 endLine 0)
      messages
        `shouldBe` [assertionMessage "given 5 should be greater or equal to 6" startLine endLine]
    it "isLowerThan should report correct message" $ do
      ((startLine, endLine), res) <- testLocation 0 $ assertThat 5 $ isLowerThan 4
      isRight res `shouldBe` False
      let (FluentTestFailure assertThatLoc messages erros success) = fromLeft undefined res
      erros `shouldBe` 1
      success `shouldBe` 0
      assertThatLoc `shouldBe` Just (SrcLoc "main" "AssertionsSpec" "test/AssertionsSpec.hs" startLine 0 endLine 0)
      messages
        `shouldBe` [assertionMessage "given 5 should be lower than 4" startLine endLine]
    it "isLowerEqualThan should report correct message" $ do
      ((startLine, endLine), res) <- testLocation 0 $ assertThat 5 $ isLowerEqualThan 4
      isRight res `shouldBe` False
      let (FluentTestFailure assertThatLoc messages erros success) = fromLeft undefined res
      erros `shouldBe` 1
      success `shouldBe` 0
      assertThatLoc `shouldBe` Just (SrcLoc "main" "AssertionsSpec" "test/AssertionsSpec.hs" startLine 0 endLine 0)
      messages
        `shouldBe` [assertionMessage "given 5 should be lower or equal to 4" startLine endLine]
    it "shouldSatisfy should report correct message" $ do
      ((startLine, endLine), res) <- testLocation 0 $ assertThat 5 $ shouldSatisfy (< 5)
      isRight res `shouldBe` False
      let (FluentTestFailure assertThatLoc messages erros success) = fromLeft undefined res
      erros `shouldBe` 1
      success `shouldBe` 0
      assertThatLoc `shouldBe` Just (SrcLoc "main" "AssertionsSpec" "test/AssertionsSpec.hs" startLine 0 endLine 0)
      messages
        `shouldBe` [assertionMessage "given 5 does not met a predicate" startLine endLine]
    it "hasSize should report correct message" $ do
      ((startLine, endLine), res) <- testLocation 0 $ assertThat [1 .. 7] $ hasSize 5
      isRight res `shouldBe` False
      let (FluentTestFailure assertThatLoc messages erros success) = fromLeft undefined res
      erros `shouldBe` 1
      success `shouldBe` 0
      assertThatLoc `shouldBe` Just (SrcLoc "main" "AssertionsSpec" "test/AssertionsSpec.hs" startLine 0 endLine 0)
      messages
        `shouldBe` [assertionMessage "expected size 5 is not equal actual size 7" startLine endLine]
    it "isEmpty should report correct message" $ do
      ((startLine, endLine), res) <- testLocation 0 $ assertThat (Just 1) $ isEmpty
      isRight res `shouldBe` False
      let (FluentTestFailure assertThatLoc messages erros success) = fromLeft undefined res
      erros `shouldBe` 1
      success `shouldBe` 0
      assertThatLoc `shouldBe` Just (SrcLoc "main" "AssertionsSpec" "test/AssertionsSpec.hs" startLine 0 endLine 0)
      messages
        `shouldBe` [assertionMessage "should be empty, but is not" startLine endLine]
    it "isNotEmpty should report correct message" $ do
      ((startLine, endLine), res) <- testLocation 0 $ assertThat Nothing $ isNotEmpty
      isRight res `shouldBe` False
      let (FluentTestFailure assertThatLoc messages erros success) = fromLeft undefined res
      erros `shouldBe` 1
      success `shouldBe` 0
      assertThatLoc `shouldBe` Just (SrcLoc "main" "AssertionsSpec" "test/AssertionsSpec.hs" startLine 0 endLine 0)
      messages
        `shouldBe` [assertionMessage "should be not empty" startLine endLine]
    it "contains should report correct message" $ do
      ((startLine, endLine), res) <- testLocation 0 $ assertThat (Just 1) $ contains 10
      isRight res `shouldBe` False
      let (FluentTestFailure assertThatLoc messages erros success) = fromLeft undefined res
      erros `shouldBe` 1
      success `shouldBe` 0
      assertThatLoc `shouldBe` Just (SrcLoc "main" "AssertionsSpec" "test/AssertionsSpec.hs" startLine 0 endLine 0)
      messages
        `shouldBe` [assertionMessage "should contain element 10, but it doesn't" startLine endLine]
    it "report timeout" $ do
      let config = defaultConfig & setAssertionTimeout 100000
      ((startLine, endLine), res) <- testLocation 0 $ assertThat' config [0 ..] $ isEqualTo [0 ..]
      isRight res `shouldBe` False
      let (FluentTestFailure assertThatLoc messages erros success) = fromLeft undefined res
      erros `shouldBe` 1
      success `shouldBe` 0
      assertThatLoc `shouldBe` Just (SrcLoc "main" "AssertionsSpec" "test/AssertionsSpec.hs" startLine 0 endLine 0)
      messages
        `shouldBe` [assertionMessage "Timeout occurred, probably some infinitive data structure or not terminating predicate has been used. Timeout: 0.1s" startLine endLine]
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
