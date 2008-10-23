module LatinSquareCommon where

import List
import Array

type Val = Int

data Move = Move (Int,Int) Val
  deriving (Eq)

solveSquare :: [c] -> (c -> [Move]) -> Grid -> Grid
solveSquare constraints satisfiers =
  fromMoves . head . EC.solve constraints satisfiers . toMoves

data Grid = Grid [[Val]]

instance Show Grid where
  show (Grid g) = concat . intersperse "\n" . map show $ g

sampleGrid = Grid [
  [0,0,3,0,2,0,6,0,0],
  [9,0,0,3,0,5,0,0,1],
  [0,0,1,8,0,6,4,0,0],
  [0,0,8,1,0,2,9,0,0],
  [7,0,0,0,0,0,0,0,8],
  [0,0,6,7,0,8,2,0,0],
  [0,0,2,6,0,9,5,0,0],
  [8,0,0,2,0,3,0,0,9],
  [0,0,5,0,1,0,3,0,0]
  ]

toMoves :: Grid -> [Move]
toMoves (Grid g) = concatMap makeMove (zip coords . concat $ g) where
  makeMove ((row,col),val) = if val == 0 then [] else [Move (row,col) val]
  coords = [(row,col) | row <- [0..8], col <- [0..8]]

fromMoves :: [Move] -> Grid
fromMoves = Grid . fromArray . array_default 0 ((0,0), (8,8)) . map makeAssoc where
  fromArray arr = [[arr ! (row,col) | col <- [0..8]] | row <- [0..8]]
  makeAssoc (Move (row,col) val) = ((row,col),val)

array_default :: (Ix i) => e -> (i, i) -> [(i,e)] -> Array i e
array_default def rng overrides =
  array rng ((zip (range rng) . repeat $ def) ++ overrides)
