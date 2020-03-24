{-# LANGUAGE ScopedTypeVariables #-}
module LibSpec (spec) where

import Lib (square, ggT)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
    describe "square" $ do
        it "calculates the square of 5.3" $
            square 5.3 `shouldBe` 28.09
        it "calculates the square of an arbitrary integer" $
            property $ \(n :: Integer) -> square n == n * n
        it "calculates the square of an arbitrary double" $
            property $ \(n :: Double) -> square n == n * n
    describe "ggT" $ do
        it "calculates the ggT of 1071 and 1029" $
            ggT 1071 1029 `shouldBe` 21
        it "calculates the ggT of an arbitrary number with 0" $
            property $ \(n :: Integer) -> ggT n 0 == abs n
        it "calculates if the ggT of two arbitrary numbers matches the gcd" $
            property $ \(a :: Integer) (b :: Integer) -> ggT a b == gcd a b