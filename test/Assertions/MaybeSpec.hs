{-# OPTIONS_GHC -Wno-type-defaults #-}

module Assertions.MaybeSpec where

import AssertionSpecUtils (testLocation)
import Data.Either (fromLeft, isRight)
import GHC.Exception
import Test.Fluent.Assertions
import Test.Fluent.Assertions.Maybe (extracting, isJust, isNothing)
import Test.Hspec (SpecWith, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = do
  describe "isJust" $ do
    it "should pass" $ do
      (_, res) <- testLocation 0 $ assertThat (Just 10) isJust
      isRight res `shouldBe` True
    it "should fail" $ do
      ((startLine, endLine), res) <- testLocation 0 $ assertThat Nothing isJust
      isRight res `shouldBe` False
      let (FluentTestFailure _ messages _ _) = fromLeft undefined res
      messages
        `shouldBe` [ ( "should be Just",
                       Just (SrcLoc "main" "Assertions.MaybeSpec" "test/Assertions/MaybeSpec.hs" startLine 0 endLine 0)
                     )
                   ]
  describe "isNothing" $ do
    it "should pass" $ do
      (_, res) <- testLocation 0 $ assertThat Nothing isNothing
      isRight res `shouldBe` True
    it "should fail" $ do
      ((startLine, endLine), res) <- testLocation 0 $ assertThat (Just 10) isNothing
      isRight res `shouldBe` False
      let (FluentTestFailure _ messages _ _) = fromLeft undefined res
      messages
        `shouldBe` [ ( "should be Nothing",
                       Just (SrcLoc "main" "Assertions.MaybeSpec" "test/Assertions/MaybeSpec.hs" startLine 0 endLine 0)
                     )
                   ]
  describe "extracting" $ do
    it "should success when is Just" $ do
      (_, res) <- testLocation 0 $ assertThat (Just 10) extracting
      isRight res `shouldBe` True
    it "should fail when given value is Nothing" $ do
      ((startLine, endLine), res) <- testLocation 0 $ assertThat Nothing extracting
      isRight res `shouldBe` False
      let (FluentTestFailure _ messages _ _) = fromLeft undefined res
      messages
        `shouldBe` [ ( "should be Just",
                       Just (SrcLoc "main" "Assertions.MaybeSpec" "test/Assertions/MaybeSpec.hs" startLine 0 endLine 0)
                     )
                   ]
    it "should execute assertion on the extracted value" $ do
      (_, res) <- testLocation 0 $ assertThat (Just 10) $ extracting . isEqualTo 10
      isRight res `shouldBe` True
    it "should execute failed assertion on the extracted value" $ do
      ((startLine, endLine), res) <- testLocation 0 $ assertThat (Just 10) $ extracting . isEqualTo 99
      isRight res `shouldBe` False
      let (FluentTestFailure _ messages _ _) = fromLeft undefined res
      messages
        `shouldBe` [ ( "given 10 should be equal to 99\n\9660\9660\n10\n\9591\n\9474\n\9589\n99\n\9650\9650\n",
                       Just (SrcLoc "main" "Assertions.MaybeSpec" "test/Assertions/MaybeSpec.hs" startLine 0 endLine 0)
                     )
                   ]