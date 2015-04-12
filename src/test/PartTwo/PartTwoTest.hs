{-# LANGUAGE  ScopedTypeVariables, TypeOperators #-}

module PartTwo.PartTwoTest where

import Test.Tasty.Hspec
import Test.Tasty
import PartTwo.PartTwo

spec :: Spec
spec = describe "maakt ne keer een sommeke" $ do
  it "kan 1 en 1 optellen" $ do
    let (seven :: Fix (Add .+ Literal)) = add (lit 3) (lit 4)
    eval seven `shouldBe` 7
    display seven `shouldBe` "3 + 4"
  it "voodoo with mul en add" $ do
    let (twentyFour :: Fix (Add .+ Literal .+ Mul)) = mul (add (lit 3) (lit 3)) (add (lit 3) (lit 1))
    eval twentyFour `shouldBe` 24
    display twentyFour `shouldBe` "3 +3 * 3 + 1"




