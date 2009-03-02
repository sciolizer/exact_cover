module Main where

import Inst.Matrix
import Inst.Queens
import Inst.Sudoku
import Array
import qualified Data.Set as S

main = do
  print $ solveMatrix $ listArray ((0,0),(2,1)) [False,True  ,  True,False   ,   True,True]
  print $ solveQueens 4
  print $ solveSudoku sampleGrid
