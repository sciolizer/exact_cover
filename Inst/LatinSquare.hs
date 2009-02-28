module Inst.LatinSquare where

import Lib.Square

solveLatinSquare :: Grid -> Grid
solveLatinSquare = solveSquare [] (\() -> [])
