module Day13
  ( ClawMachine(ClawMachine)
  , findCheapestWinForAllPrizes
  ) where

import Data.Function (on)
import Data.List (minimumBy)
import Data.Massiv.Array hiding (map, sum)
import Data.Maybe (maybeToList)

data ClawMachine =
  ClawMachine
    { aButton, bButton, prize :: (Int, Int)
    }

makeMatrix :: ClawMachine -> Matrix B (Int, Int)
makeMatrix clawMachine =
  let size = getSizeOfArray clawMachine
      ClawMachine (aX, aY) (bX, bY) _ = clawMachine
      getNextElement :: (Int, Int) -> Ix2 -> (Int, Int)
      getNextElement _ (row :. 0) = (row * bX, row * bY)
      getNextElement (x, y) _ = (x + aX, y + aY)
   in computeAs B $ iiterateN size getNextElement (0, 0)

getSizeOfArray :: ClawMachine -> Sz2
getSizeOfArray (ClawMachine (aX, aY) (bX, bY) (pX, pY)) =
  let aMax = max (pX `div` aX) (pY `div` aY)
      bMax = max (pX `div` bX) (pY `div` bY)
   in Sz2 (bMax + 1) (aMax + 1)

findApproaches :: ClawMachine -> [Ix2]
findApproaches clawMachine =
  let array = makeMatrix clawMachine
      prizes =
        ifoldSemi
          (\ix value ->
             if value == prize clawMachine
               then [ix]
               else [])
          []
          array
   in prizes

findCheapestApproach :: ClawMachine -> Maybe (Ix2, Int)
findCheapestApproach clawMachine =
  case findApproaches clawMachine of
    [] -> Nothing
    approaches ->
      let getPrice :: Ix2 -> Int
          getPrice (bNumber :. aNumber) = aNumber * 3 + bNumber
          approachesWithPrice = map (\ix -> (ix, getPrice ix)) approaches
       in Just $ minimumBy (compare `on` snd) approachesWithPrice

findCheapestWinForAllPrizes :: [ClawMachine] -> Int
findCheapestWinForAllPrizes clawMachines =
  let prices = do
        clawMachine <- clawMachines
        (_, cost) <- maybeToList (findCheapestApproach clawMachine)
        pure cost
   in sum prices
