module Test.MultiplicationTest where

import Test.Hspec
import Test.QuickCheck
import qualified Multiplication

run :: IO ()
run = hspec $ do
  describe "Multiplication" $ do
    it "5 * 5 = 25" $ do
      Multiplication.multiply 5 5 `shouldBe` 25
    it "0 * 5 = 0" $ do
      Multiplication.multiply 0 5 `shouldBe` 0
    it "-2 * 5 = -10" $ do
      Multiplication.multiply (-2) 5 `shouldBe` -10
    it "2 * -5 = -10" $ do
      Multiplication.multiply 2 (-5) `shouldBe` -10