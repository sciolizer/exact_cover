module Inst.Matrix where

-- The ExactCover problem, as it is usually presented - 
-- a matrix of booleans where a subset of rows must be chosen so that
-- each column has exactly one true value.

import ExactCover
import Data.Array

type Matrix = (Array (Int,Int) Bool)

data Constraint = Column Int deriving (Eq,Show,Ord)

constraints :: Matrix -> [Constraint]
constraints matrix =
  let (l_bound, u_bound) = bounds matrix
  in [Column c | c <- [(snd l_bound)..(snd u_bound)]]

data Satisfier = Row Int deriving (Eq,Show,Ord)

satisfiers :: Matrix -> Constraint -> [Satisfier]
satisfiers matrix (Column c) =
  let (l_bound, u_bound) = bounds matrix
  in [Row r | r <- [(fst l_bound)..(fst u_bound)], matrix ! (r, c)]

type Soln = [Satisfier]

solveMatrix :: Matrix -> [Soln]
solveMatrix m = solve (constraints m) (satisfiers m) []

-- [ 0 1
--   1 0 ]
example :: Matrix
example = array ((0, 0), (1, 1)) [((0, 0), False), ((0, 1), True), ((1, 0), True), ((1, 1), False)]
