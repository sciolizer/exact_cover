module Lib.Square where

import List
import Array

import ExactCover
import Lib.ArrayDefault

data Constraint c =
    RowColumn Int Int
  | RowNumber Int Val
  | ColNumber Int Val
  | Other c
  deriving (Eq,Show,Ord)

constraints :: [c] -> [Constraint c]
constraints others =
    -- every cell must have a value
    [RowColumn row col | row <- [0..8], col <- [0..8]]
    -- every row must have the numbers 1 through 9
 ++ [RowNumber row val | row <- [0..8], val <- [1..9]]
    -- every column must have the numbers 1 through 9
 ++ [ColNumber col val | col <- [0..8], val <- [1..9]]
 ++ map Other others
    
satisfiers :: (c -> [Satisfier]) -> Constraint c -> [Satisfier]
satisfiers others = satisfiers' where
  satisfiers' (RowColumn row col) = [Move (row,col) val | val <- [1..9]]
  satisfiers' (RowNumber row val) = [Move (row,col) val | col <- [0..8]]
  satisfiers' (ColNumber col val) = [Move (row,col) val | row <- [0..8]]
  satisfiers' (Other c) = others c
 
type Val = Int

data Satisfier = Move (Int,Int) Val
  deriving (Eq,Show,Ord)

--solveSquare :: (Eq c) => [c] -> (c -> [Satisfier]) -> Grid -> Grid
solveSquare c s = fromMoves . head . solve (constraints c) (satisfiers s) . toMoves
fromRight (Right x) = x

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

toMoves :: Grid -> [Satisfier]
toMoves (Grid g) = concatMap makeMove (zip coords . concat $ g) where
  makeMove ((row,col),val) = if val == 0 then [] else [Move (row,col) val]
  coords = [(row,col) | row <- [0..8], col <- [0..8]]

fromMoves :: [Satisfier] -> Grid
fromMoves = Grid . fromArray . array_default 0 ((0,0), (8,8)) . map makeAssoc where
  fromArray arr = [[arr ! (row,col) | col <- [0..8]] | row <- [0..8]]
  makeAssoc (Move (row,col) val) = ((row,col),val)
