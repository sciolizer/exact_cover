module Main where

import Square

data BoxConstraint =
  BoxNumber (Int,Int) Val  -- (Int,Int) is coords of upper left cell in box
  deriving (Eq)

otherConstraints :: [Constraint BoxConstraint]
otherConstraints =
  -- every box must have the numbers 1 through 9
  [BoxNumber (row,col) val | row <- [0,3,6], col <- [0,3,6], val <- [1..9]]
    
otherSatisfiers :: BoxConstraint -> [Move]
otherSatisfiers (BoxNumber (r_i,c_i) val) =
  [Move (r+r_i,c+c_i) val | r <- [0..2], c <- [0..2]]
 
solveSudoku :: Grid -> Grid
solveSudoku = solveSquare otherConstraints otherSatisfiers

main = print $ solveSudoku sampleGrid
