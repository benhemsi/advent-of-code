module Day4Spec where

import Day4 (findWord)
import Test.Hspec

testData =
  [ "MMMSXXMASM"
  , "MSAMXMSMSA"
  , "AMXSXMAAMM"
  , "MSAMASMSMX"
  , "XMASAMXAMM"
  , "XXAMMXXAMA"
  , "SMSMSASXSS"
  , "SAXAMASAAA"
  , "MAMMMXMMMM"
  , "MXMXAXMASX"
  ]

spec = do
  describe "findWord" $ do
    it "return all the occurrences of the word" $ do
      findWord "XMAS" testData `shouldBe` Right []
