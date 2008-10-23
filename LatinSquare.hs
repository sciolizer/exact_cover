module Main where

import Square

solveLatinSquare :: Grid -> Grid
solveLatinSquare = solveSquare [] (\() -> [])

main = print $ solveLatinSquare sampleGrid
