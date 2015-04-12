module Main where

import Test.Tasty.Hspec
import Test.Tasty

import qualified PartTwo.PartTwoTest as PartTwoTest
import qualified PartThree.PartThreeTest as PartThreeTest

main :: IO ()
main = do
  partTwo <- testSpec "part two" PartTwoTest.spec
  partThree <- testSpec "part three" PartThreeTest.spec
  let unitTests = testGroup "unit tests" [partTwo, partThree]
  defaultMain unitTests