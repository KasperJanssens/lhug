module PartThree.PartThreeTest where

import Test.Tasty.Hspec
import PartThree.PartThree

spec :: Spec
spec = describe "Part Three" $ do
         it "should revert" $ do
           let originalList = Cons 0 (Cons 1 (Cons 2 Nil))
           let expectedList = Cons 2 (Cons 1 (Cons 0 Nil))
           let actualList = reversef originalList
           actualList `shouldBe` expectedList
         it "should append" $ do
           let leftList = Cons 0 (Cons 1 Nil)
           let rightList = Cons 2 Nil
           let expectedList = Cons 0 (Cons 1 (Cons 2 Nil))
           let actualList = appendf leftList rightList
           actualList `shouldBe` expectedList
         it "should be id" $ do
           let expectedList = Cons 0 (Cons 1 (Cons 2 Nil))
           let actualList = idList expectedList
           actualList `shouldBe` expectedList