module Day13Spec where

import Day13
import Test.Hspec

testData =
  [ ClawMachine (94, 34) (22, 67) (8400, 5400)
  , ClawMachine (26, 66) (67, 21) (12748, 12176)
  , ClawMachine (17, 86) (84, 37) (7870, 6450)
  , ClawMachine (69, 23) (27, 71) (18641, 10279)
  ]

spec = do
  describe "findCheapestWinForAllPrizes" $ do
    it "calculate the cheapest cost to win all the prizes" $ do
      findCheapestWinForAllPrizes testData `shouldBe` 480
