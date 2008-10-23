module Main where

import List
import Array

import qualified ExactCover as EC

type Val = Int

data Constraint =
    RowColumn Int Int
  | RowNumber Int Val
  | ColNumber Int Val
  | BoxNumber (Int,Int) Val  -- (Int,Int) is coords of upper left cell in box
  deriving (Eq)

data Move = Move (Int,Int) Val
  deriving (Eq)

constraints :: [Constraint]
constraints =
    -- every cell must have a value
    [RowColumn row col | row <- [0..8], col <- [0..8]]
    -- every row must have the numbers 1 through 9
 ++ [RowNumber row val | row <- [0..8], val <- [1..9]]
    -- every column must have the numbers 1 through 9
 ++ [ColNumber col val | col <- [0..8], val <- [1..9]]
    -- every box must have the numbers 1 through 9
 ++ [BoxNumber (row,col) val | row <- [0,3,6], col <- [0,3,6], val <- [1..9]]
    
satisfiers :: Constraint -> [Move]
satisfiers (RowColumn row col) = [Move (row,col) val | val <- [1..9]]
satisfiers (RowNumber row val) = [Move (row,col) val | col <- [0..8]]
satisfiers (ColNumber col val) = [Move (row,col) val | row <- [0..8]]
satisfiers (BoxNumber (r_i,c_i) val) =
  [Move (r+r_i,c+c_i) val | r <- [0..2], c <- [0..2]]
 
solveSudoku :: Grid -> Grid
solveSudoku = fromMoves . head . EC.solve constraints satisfiers . toMoves

data Grid = Grid [[Val]]

instance Show Grid where
  show (Grid g) = concat . intersperse "\n" . map show $ g

main = print $ solveSudoku sampleGrid

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
