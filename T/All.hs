module Main where

import Inst.LatinSquare
import Inst.Matrix
import Inst.Queens
import Inst.Set
import Inst.Sudoku
import Lib.Square
import Array
import qualified Data.Set as S

main = do
  print $ solveLatinSquare sampleGrid
  print $ solveMatrix $ Matrix $ listArray ((0,0),(3,2)) [False,True  ,  True,False   ,   True,True]
  print $ solveQueens 4
  print $ solveSet (SetProblem (S.fromList [1,2,3]) (S.fromList [S.fromList [1], S.fromList [1,2], S.fromList [3]]))
  print $ solveSudoku sampleGrid
