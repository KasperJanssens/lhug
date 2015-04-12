module PartThree.PartThreeTest where

import Test.Tasty.Hspec
import PartThree.PartThree

spec :: Spec
spec = describe "PartThree" $ do
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
         it "should calculate the length" $ do
           let listAccent = In (Cons' 0 (In (Cons' 0 (In (Cons' 0 (In Nil'))))))
           length3 listAccent `shouldBe` 3
         it "should show the expression" $ do
           let expr = In (Add' (In (Lit' 7)) (In(Lit' 8)))
           showExp2 expr `shouldBe` "(7)+(8)"
         it "tailored fold function can eval" $ do
           let expr = Add (Add (Lit 7) (Lit 8)) (Add (Lit 6) (Lit 3))
           eval3 expr `shouldBe` 24
         it "generic id function" $ do
           let expectedExpr = In (Add' (In (Lit' 7)) (In(Lit' 8)))
           showExp2 (gId expectedExpr) `shouldBe` showExp2 expectedExpr