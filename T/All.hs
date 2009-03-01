module Main where

import Inst.Matrix
import Inst.Queens
import Inst.Sudoku
import Lib.Square
import Array
import qualified Data.Set as S

main = do
  print $ solveMatrix $ Matrix $ listArray ((0,0),(2,1)) [False,True  ,  True,False   ,   True,True]
  print $ solveQueens 5
  print $ solveSudoku sampleGrid