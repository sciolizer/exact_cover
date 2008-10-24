module PentominosTest where

import Pentominos
import Test.QuickCheck
import Array
import Monad

flipTest (x,y) = (x,y) == flp True (flp True (x,y))

rotateTest (x,y) = all areEquiv rotationEquivalences where
  areEquiv (seq1, seq2) = composeRotations seq1 (x,y) == composeRotations seq2 (x,y)

composeRotations :: [Rotation] -> (Int,Int) -> (Int,Int)
composeRotations [] z = z
composeRotations (x:xs) z = rotate x (composeRotations xs z)

rotationEquivalences = [
  ([Ninety,Ninety,Ninety,Ninety], [Zero]),
  ([Ninety,Ninety], [OneEighty]),
  ([Ninety,Ninety,Ninety], [TwoSeventy]),
  ([TwoSeventy,TwoSeventy], [OneEighty,Zero])
  ]

boundedEnumTest = 8 == length (boundedEnum :: [Position])

enumTest :: Position -> Bool
enumTest pos = toEnum (fromEnum pos) == pos
enumTestRev n = let x = n `mod` 8 in fromEnum ((toEnum x) :: Position) == x

constraintsLength brd@(Board arr) = length (constraints brd) == 12 + length (range . bounds $ arr)

checkAll = do 
  quickCheck flipTest
  quickCheck rotateTest
  quickCheck enumTest
  quickCheck enumTestRev
  -- quickCheck constraintsLength
  quickCheck boundedEnumTest


{-
instance Arbitrary Board where
  arbitrary = return . Board =<< (arbitrary :: (Gen (Array (Int,Int) Bool)))
  coarbitrary = liftM Board
-}

instance Arbitrary Position where
  arbitrary = do
    r <- arbitrary
    b <- arbitrary
    return $ Position r b

instance Arbitrary Rotation where
  arbitrary = elements boundedEnum

{-
instance (Enum e, Bounded e) => Arbitrary e where
  arbitrary = elements boundedEnum
-}
