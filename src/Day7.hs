module Day7
  ( getSumOfValidRows
  ) where

import Control.Monad (guard)
import Data.Foldable (foldl')

data SumProductTree
  = EmptyTree
  | Leaf Int
  | Node SumProductTree SumProductTree
  deriving (Eq, Show)

growTree :: SumProductTree -> Int -> SumProductTree
growTree EmptyTree x = Leaf x
growTree (Leaf x) y = Node (Leaf (x + y)) (Leaf (x * y))
growTree (Node sumSide productSide) x =
  Node (growTree sumSide x) (growTree productSide x)

createTree :: [Int] -> SumProductTree
createTree = foldl' growTree EmptyTree

treeContainsValue :: Int -> SumProductTree -> Bool
treeContainsValue _ EmptyTree = False
treeContainsValue x (Leaf y) = x == y
treeContainsValue x (Node sumSide productSide) =
  treeContainsValue x sumSide || treeContainsValue x productSide

getSumOfValidRows :: [(Int, [Int])] -> Int
getSumOfValidRows xss =
  sum $ do
    (target, xs) <- xss
    let tree = createTree xs
    guard (treeContainsValue target tree)
    pure target
