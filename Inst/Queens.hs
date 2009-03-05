module Inst.Queens where

-- The N queens problem.
-- Uses GeneralizedExactCover, because it is not required
-- for each diagonal to contain a piece.

import Lib.GeneralizedExactCover

data Constraint =
    Row Int
  | Column Int
  | ForwardDiagonal Int -- (/) int is sum of the row and column
  | BackDiagonal Int -- (\) int is difference of the row and column
  deriving (Eq,Show,Ord)

data Satisfier = Move Int Int -- row, col
  deriving (Eq,Show,Ord)

required, optional :: Int -> [Constraint]
required (n+1) = [con val | con <- [Row, Column], val <- [0..n]]
optional (n+1) =
    [ForwardDiagonal val | val <- [0..(2*n)]]
 ++ [BackDiagonal    val | val <- [(-1*n)..n]]

satisfiers :: Int -> Constraint -> [Satisfier]
satisfiers (n+1) c =
  case c of
    Row row ->             [Move row col | col <- [0..n]]
    Column col ->          [Move row col | row <- [0..n]]
    ForwardDiagonal sum -> [Move row (sum - row) | row <- [(max 0 (sum - n))..(min n sum)]]
    BackDiagonal diff ->   [Move row (row - diff) | row <- [(max 0 diff)..(min n (n + diff))]]

solveQueens n = solveGeneralized (required n) (optional n) (satisfiers n) []

