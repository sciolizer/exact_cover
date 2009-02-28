module Inst.Matrix where

import ExactCover
import Array

data Matrix = Matrix (Array (Int,Int) Bool) deriving (Show,Eq)

data Constraint = Column Int deriving (Eq,Show,Ord)

constraints :: Matrix -> [Constraint]
constraints (Matrix arr) =
  let (l_bound, u_bound) = bounds arr
  in [Column c | c <- [(snd l_bound)..(snd u_bound)]]

data Satisfier = Row Int deriving (Eq,Show,Ord)

satisfiers :: Matrix -> Constraint -> [Satisfier]
satisfiers (Matrix arr) (Column c) =
  let (l_bound, u_bound) = bounds arr
  in [Row r | r <- [(fst l_bound)..(fst u_bound)], arr ! (r, c)]

type Soln = [Int] -- list of rows

-- solveMatrix :: Matrix -> [Soln] -- list of rows
solveMatrix m = solve (constraints m) (satisfiers m) []
