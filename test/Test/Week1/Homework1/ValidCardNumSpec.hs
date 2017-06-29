module Test.Week1.Homework1.ValidCardNumSpec (spec) where

import Test.Hspec

import Week1.Homework1.ValidCardNum
  ( toDigits
  , toDigitsRev
  , doubleEveryOther
  , sumDigits
  , validate)


spec :: Spec
spec = do
  describe "Exercise 1. Find digits of a number" $ do
    it "toDigits 0" $ do
      toDigits 0 `shouldBe` []

    it "toDigits 1234" $ do
     toDigits 1234 `shouldBe` [1,2,3,4]

    it "toDigits (-17)" $ do
      toDigits (-17) `shouldBe` []

    it "toDigitsRev 1234" $ do
      toDigitsRev 1234 `shouldBe` [4,3,2,1]

  describe "Exercise 2. Double every other digit." $ do
    it "doubleEveryOther [8,7,6,5]" $ do
      doubleEveryOther [5, 6, 7, 8] `shouldBe` [5, 12, 7, 16]
    it "doubleEveryOther [1,2,3]" $ do
      doubleEveryOther [1,2,3] `shouldBe` [1,4,3]

  describe "Exercies 3. mix of one-digit and two-digit numbers." $ do
    it "sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22" $ do
      sumDigits [16,7,12,5] `shouldBe` 22

  describe "Validate" $ do
    it "validate" $ do
      validate 4012888888881881 `shouldBe` True

    it "validate2" $ do
      validate 4012888888881882 `shouldBe` False
