    $ ghci
    GHCi, version 7.4.1: http://www.haskell.org/ghc/  :? for help
    Loading package ghc-prim ... linking ... done.
    Loading package integer-gmp ... linking ... done.
    Loading package base ... linking ... done.
    Prelude> :load Inst.Queens
    [1 of 3] Compiling ExactCover       ( ExactCover.hs, interpreted )
    [2 of 3] Compiling Lib.GeneralizedExactCover ( Lib/GeneralizedExactCover.hs, interpreted )
    [3 of 3] Compiling Inst.Queens      ( Inst/Queens.hs, interpreted )
    Ok, modules loaded: Inst.Queens, Lib.GeneralizedExactCover, ExactCover.
    *Inst.Queens> solveQueens 4
    Loading package array-0.4.0.0 ... linking ... done.
    Loading package deepseq-1.3.0.0 ... linking ... done.
    Loading package containers-0.4.2.1 ... linking ... done.
    [[Move 0 1,Move 1 3,Move 2 0,Move 3 2],[Move 0 2,Move 1 0,Move 2 3,Move 3 1]]

There are also matrix, sudoku, and pentominos examples.

They might have bitrotted, though.
