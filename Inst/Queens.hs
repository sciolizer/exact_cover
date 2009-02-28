module Inst.Queens where

import ExactCover

data Constraint =
    Row Int
  | Column Int
  | ForwardDiagonal Int -- (/) value is sum of the row and column
  | BackDiagonal Int -- (\) value is difference of the row and column
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
satisfiers n (Row row) = [Move row col | col <- [0..(n-1)]]
satisfiers n (Column col) = [Move row col | row <- [0..(n-1)]]
satisfiers n (ForwardDiagonal sum) = [Move row (sum - row) | row <- [0..(n-1)]]
satisfiers n (BackDiagonal diff) = [Move row (row + diff) | row <- [0..(n-1)]]

solveQueens n = solve (constraints n) (satisfiers n) []
