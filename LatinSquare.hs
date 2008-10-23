module Main where

import List
import Array

import qualified ExactCover as EC

data Constraint =
    RowColumn Int Int
  | RowNumber Int Val
  | ColNumber Int Val
  | BoxNumber (Int,Int) Val  -- (Int,Int) is coords of upper left cell in box
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
solveSudoku = solveSquare constraints satisfiers

main = print $ solveSudoku sampleGrid
