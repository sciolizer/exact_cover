module Square where

import List
import Array

import ExactCover

data Constraint c =
    RowColumn Int Int
  | RowNumber Int Val
  | ColNumber Int Val
  | Other c
  deriving (Eq)

constraints :: [c] -> [Constraint c]
constraints others =
    -- every cell must have a value
    [RowColumn row col | row <- [0..8], col <- [0..8]]
    -- every row must have the numbers 1 through 9
 ++ [RowNumber row val | row <- [0..8], val <- [1..9]]
    -- every column must have the numbers 1 through 9
 ++ [ColNumber col val | col <- [0..8], val <- [1..9]]
 ++ map Other others
    
satisfiers :: (c -> [Move]) -> Constraint c -> [Move]
satisfiers others = satisfiers' where
  satisfiers' (RowColumn row col) = [Move (row,col) val | val <- [1..9]]
  satisfiers' (RowNumber row val) = [Move (row,col) val | col <- [0..8]]
  satisfiers' (ColNumber col val) = [Move (row,col) val | row <- [0..8]]
  satisfiers' (Other c) = others c
 
type Val = Int

data Move = Move (Int,Int) Val
  deriving (Eq)

solveSquare :: (Eq c) => [c] -> (c -> [Move]) -> Grid -> Grid
solveSquare c s = fromMoves . head . solve (constraints c) (satisfiers s) . toMoves

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
