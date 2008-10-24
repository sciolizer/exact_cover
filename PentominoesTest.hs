module PentominosTest where

import Pentominos

flipTest (x,y) = (x,y) == flip True (flip True (x,y))

rotateTest (x,y) = all areEquiv rotationEquivalences where
  areEquiv (seq1, seq2) = composeRotations seq1 (x,y) == composeRotations seq2 (x,y)

composeRotations :: [Rotation] -> (Int,Int) -> (Int,Int)
composeRotations [] = id
composeRotaitons (x:xs) z = rotate x (composeRotations xs z)

rotationEquivalences = [
  ([Ninety,Ninety,Ninety,Ninety], [Zero]),
  ([Ninety,Ninety], [OneEighty]),
  ([Ninety,Ninety,Ninety], [TwoSeventy]),
  ([TwoSeventy,TwoSeventiy], [OneEighty,Zero])
  ]
