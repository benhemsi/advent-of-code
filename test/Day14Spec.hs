module Day14Spec where

import Day14
import Test.Hspec

testRobots =
  [ Robot (0, 4) (3, -3)
  , Robot (6, 3) (-1, -3)
  , Robot (10, 3) (-1, 2)
  , Robot (2, 0) (2, -1)
  , Robot (0, 0) (1, 3)
  , Robot (3, 0) (-2, -2)
  , Robot (7, 6) (-1, -3)
  , Robot (3, 0) (-1, -2)
  , Robot (9, 3) (2, 3)
  , Robot (7, 3) (-1, 2)
  , Robot (2, 4) (2, -3)
  , Robot (9, 5) (-3, -3)
  ]

spec = do
  describe "moveRobots" $ do
    it "moveRobots the provided number of times" $ do
      moveRobots 100 (101, 103) testRobots `shouldBe` []
