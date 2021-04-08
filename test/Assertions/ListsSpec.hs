{-# OPTIONS_GHC -Wno-type-defaults #-}

module Assertions.ListsSpec where

import AssertionSpecUtils (assertionMessage, testLocation)
import Data.Either (fromLeft, isLeft, isRight)
import Test.Fluent.Assertions
  ( FluentTestFailure (FluentTestFailure),
  )
import Test.Fluent.Assertions.Core
  ( assertThat,
  )
import Test.Fluent.Assertions.List
  ( shouldHaveSameElements,
    shouldStartWith,
  )
import Test.Hspec
  ( SpecWith,
    describe,
    hspec,
    it,
    shouldBe,
  )

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = do
  describe "lists" $ do
    describe "shouldStartWith" $ do
      it "fail once prefix has more elements than given list" $ do
        ((startLine, endLine), res) <- testLocation 0 $ assertThat [0 .. 9] $ shouldStartWith [0 .. 10]
        isLeft res `shouldBe` True
        let (FluentTestFailure _ messages _ _) = fromLeft undefined res
        messages
          `shouldBe` [assertionMessage "the lenght of given list is 10, but should be lower or equal 11" startLine endLine]
      it "fail once prefix is not same as expected" $ do
        ((startLine, endLine), res) <- testLocation 0 $ assertThat [0 .. 9] $ shouldStartWith [1 .. 10]
        isLeft res `shouldBe` True
        let (FluentTestFailure _ messages _ _) = fromLeft undefined res
        messages
          `shouldBe` [assertionMessage "should start with [1,2,3,4,5,6,7,8,9,10], but it start with [0,1,2,3,4,5,6,7,8,9]" startLine endLine]
      it "success once prefix match expected one" $ do
        (_, res) <- testLocation 0 $ assertThat [0 .. 9] $ shouldStartWith [0 .. 9]
        isRight res `shouldBe` True
    describe "shouldHaveExactSameElements" $ do
      it "fail once two lists have different lenght" $ do
        ((startLine, endLine), res) <- testLocation 0 $ assertThat [0 .. 1000] $ shouldHaveSameElements [500 .. 50090]
        isLeft res `shouldBe` True
        let (FluentTestFailure _ messages _ _) = fromLeft undefined res
        messages
          `shouldBe` [assertionMessage "should have same leght, but they don't. Given: 1001 is not equal to expected 49591" startLine endLine]
      it "fail once list don't have same elements as expected list" $ do
        ((startLine, endLine), res) <- testLocation 0 $ assertThat [1, 2, 3] $ shouldHaveSameElements [1, 2, 4]
        isLeft res `shouldBe` True
        let (FluentTestFailure _ messages _ _) = fromLeft undefined res
        messages
          `shouldBe` [assertionMessage "two lists should have same elements but:\n given list don't have [4]\nexpected list don't have [3]" startLine endLine]
      it "success once two lists are equal" $ do
        let list = [1 .. 100]
        (_, res) <- testLocation 0 $ assertThat list $ shouldHaveSameElements list
        isRight res `shouldBe` True
      it "success once two lists have same elements in different order" $ do
        (_, res) <- testLocation 0 $ assertThat [1, 2, 3, 4, 5] $ shouldHaveSameElements [5, 2, 1, 3, 4]
        isRight res `shouldBe` True
