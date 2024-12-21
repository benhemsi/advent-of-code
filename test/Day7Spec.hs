module Day7Spec where

import Day7 (getSumOfValidRows)
import Test.Hspec

testData =
  [ (190, [10, 19])
  , (3267, [81, 40, 27])
  , (83, [17, 5])
  , (156, [15, 6])
  , (7290, [6, 8, 6, 15])
  , (161011, [16, 10, 13])
  , (192, [17, 8, 14])
  , (21037, [9, 7, 18, 13])
  , (292, [11, 6, 16, 20])
  ]

spec = do
  describe "getSumOfValidRows" $ do
    it
      "return the sum of all the values that can be created by + and * the values together" $ do
      getSumOfValidRows testData `shouldBe` 3749
