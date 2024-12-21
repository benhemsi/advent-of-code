{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day4 where

import Control.Monad (guard)
import Data.Array.Unboxed
import Data.List (group)

type WordSearchBoard = UArray (Int, Int) Char

data Direction
  = North
  | NorthEast
  | East
  | SouthEast
  | South
  | SouthWest
  | West
  | NorthWest
  deriving (Eq, Enum, Bounded, Show)

data WordFound =
  WordFound
    { coordinate :: (Int, Int)
    , direction :: Direction
    }
  deriving (Eq, Show)

createArray :: [String] -> Either String WordSearchBoard
createArray [] = Left "Empty word search board"
createArray strings
  | "" `elem` strings = Left "Empty row"
  | checkStringsAllSameLength strings = Left "Strings not all same length"
  | otherwise = Right arr
  where
    minIx = (0, 0)
    maxIx = (length (head strings) - 1, length strings - 1)
    arr = listArray (minIx, maxIx) (concat strings)

checkStringsAllSameLength :: [String] -> Bool
checkStringsAllSameLength strings = length (group lengthOfStrings) > 1
  where
    lengthOfStrings = map length strings

findWordOccurrences :: [String] -> String -> Either String [WordFound]
findWordOccurrences _ "" = Right []
findWordOccurrences _ _ = undefined

checkArrayIndex :: WordSearchBoard -> CharWithCoor -> Bool
checkArrayIndex board (c, ix) = inRange (bounds board) ix && (board ! ix == c)

iterateOnDirection :: Direction -> (Int, Int) -> (Int, Int)
iterateOnDirection North (a, b) = (a, b - 1)
iterateOnDirection NorthEast (a, b) = (a + 1, b - 1)
iterateOnDirection East (a, b) = (a + 1, b)
iterateOnDirection SouthEast (a, b) = (a + 1, b + 1)
iterateOnDirection South (a, b) = (a, b - 1)
iterateOnDirection SouthWest (a, b) = (a - 1, b + 1)
iterateOnDirection West (a, b) = (a - 1, b)
iterateOnDirection NorthWest (a, b) = (a - 1, b - 1)

type CharWithCoor = (Char, (Int, Int))

getIndicesToCheck :: String -> (Int, Int) -> Direction -> [CharWithCoor]
getIndicesToCheck wordToSearchFor startingIndex direction =
  loop wordToSearchFor startingIndex []
  where
    loop :: String -> (Int, Int) -> [CharWithCoor] -> [CharWithCoor]
    loop [] _ acc = acc
    loop (c:cs) coor acc =
      loop cs (iterateOnDirection direction coor) ((c, coor) : acc)

checkSquare :: WordSearchBoard -> String -> (Int, Int) -> [WordFound]
checkSquare board wordToSearchFor@(c:_) coor =
  if board ! coor == c
    then do
      direction <- [minBound, maxBound]
      let indices = getIndicesToCheck wordToSearchFor coor direction
      guard (all (checkArrayIndex board) indices)
      [WordFound coor direction]
    else []
checkSquare _ _ _ = []

searchForWord :: String -> WordSearchBoard -> [WordFound]
searchForWord wordToSearchFor board = do
  ix <- indices board
  checkSquare board wordToSearchFor ix

findWord :: String -> [String] -> Either String [WordFound]
findWord wordToSearchFor board =
  fmap (searchForWord wordToSearchFor) (createArray board)
