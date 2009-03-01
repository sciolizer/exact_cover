module Inst.Queens where

import ExactCover

data Constraint =
    Row Int
  | Column Int
  | ForwardDiagonal Int -- (/) int is sum of the row and column
  | BackDiagonal Int -- (\) int is difference of the row and column
  deriving (Eq,Show,Ord)

data Satisfier = Move Int Int -- row, col
  deriving (Eq,Show,Ord)

constraints :: Int -> [Constraint]
constraints n =
    [con val | val <- [0..(n-1)], con <- [Row, Column]]
 ++ [con val | val <- [0..(2*(n-1))], con <- [ForwardDiagonal, BackDiagonal]]

-- The really big question here is... how do I get the compiler to infer
-- this function? At least in the case of Row and Column
-- (and in the case of RowColumn, RowNumber, and ColNumber in the Sudoku problem)
-- the constraint is just a Satisfier with a value missing. What makes it non
-- inferable is things like the Diagonals (and in Sudoku, the box constraint)
-- I bet there's a way to do it in Prolog.
satisfiers :: Int -> Constraint -> [Satisfier]
satisfiers (n+1) c =
  case c of
    Row row ->             [Move row col | col <- [0..n]]
    Column col ->          [Move row col | row <- [0..n]]
    ForwardDiagonal sum -> [Move row (sum - row) | row <- [(max 0 (sum - n))..(min n sum)]]
    BackDiagonal diff ->   [Move row (row - diff) | row <- [(max 0 diff)..(min n (n + diff))]]

prop_fd = satisfiers 5 (ForwardDiagonal 0) == [Move 0 0]
prop_fd_big = satisfiers 5 (ForwardDiagonal 8) == [Move 4 4]
prop_fd_mid = satisfiers 5 (ForwardDiagonal 2) == [Move 0 2, Move 1 1, Move 2 0]

prop_bd = satisfiers 5 (BackDiagonal 2) == [Move 2 0, Move 3 1, Move 4 2]
prop_bd_zero = satisfiers 5 (BackDiagonal 0) == [Move x x | x <- [0..4]]
prop_bd_neg = satisfiers 5 (BackDiagonal (-4)) == [Move 0 4]


solveQueens n = solve (constraints n) (satisfiers n) []

-- 01234 (negative)
-- 10123
-- 21012
-- 32101
-- 43210
-- (positive)

-- 01234
-- 12345
-- 23456
-- 34567
-- 45678

