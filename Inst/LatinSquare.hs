module Inst.LatinSquare where

import Lib.Square

-- actually, I'm think it might be best to just let
-- this stand as is, and then WRAP the functions
-- to do the Sudoku variation. That way there
-- will be no "OTHER" constructor.
-- (It's more boilerplate, but it's easier to understand,
--   and in fact the wrapping could probably be made simpler
--   by using HOFs.)
solveLatinSquare :: Grid -> Grid
solveLatinSquare = solveSquare [] (\() -> [])
