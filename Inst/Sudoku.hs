module Inst.Sudoku where

import List
import Array

import ExactCover

type Val = Int

data Constraint c =
    RowColumn Int Int
  | RowNumber Int Val
  | ColNumber Int Val
  | BoxNumber (Int,Int) Val -- (Int,Int) is coords of upper left cell in box
  deriving (Eq,Show,Ord)

constraints :: [Constraint c]
constraints =
    -- every cell must have a value
    [RowColumn row col | row <- [0..8], col <- [0..8]]
    -- every row must have the numbers 1 through 9
 ++ [RowNumber row val | row <- [0..8], val <- [1..9]]
    -- every column must have the numbers 1 through 9
 ++ [ColNumber col val | col <- [0..8], val <- [1..9]]
   -- every box must have the numbers 1 through 9
 ++ [BoxNumber (row,col) val | row <- [0,3,6], col <- [0,3,6], val <- [1..9]]

data Satisfier = Move (Int,Int) Val
  deriving (Eq,Show,Ord)

satisfiers :: Constraint c -> [Satisfier]
satisfiers constraint =
  case constraint of
    RowColumn row col       -> [Move (row,col) val | val <- [1..9]]
    RowNumber row val       -> [Move (row,col) val | col <- [0..8]]
    ColNumber col val       -> [Move (row,col) val | row <- [0..8]]
    BoxNumber (row,col) val -> [Move (r+row,c+col) val | r <- [0..2], c <- [0..2]]
 
type Grid = [[Val]]

toMoves :: Grid -> [Satisfier]
toMoves g = concatMap makeMove (zip coords . concat $ g) where
  makeMove ((row,col),val) = if val == 0 then [] else [Move (row,col) val]
  coords = [(row,col) | row <- [0..8], col <- [0..8]]

fromMoves :: [Satisfier] -> Grid
fromMoves = fromArray . array_default 0 ((0,0), (8,8)) . map makeAssoc where
  fromArray arr = [[arr ! (row,col) | col <- [0..8]] | row <- [0..8]]
  makeAssoc (Move (row,col) val) = ((row,col),val)

array_default :: (Ix i) => e -> (i, i) -> [(i,e)] -> Array i e
array_default def rng overrides =
  array rng $ zip (range rng) (repeat def) ++ overrides

solveSudoku :: Grid -> [Grid]
solveSudoku = map fromMoves . solve constraints satisfiers . toMoves
