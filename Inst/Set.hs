module Inst.Set where

import qualified Data.Set as S
import Data.Set (Set)
import ExactCover

data Constraint a = Covered a deriving (Eq,Show,Ord)

data Satisfier a = Include (Set a) deriving (Eq,Show,Ord)

data SetProblem a = SetProblem (Set a) (Set (Set a))

constraints :: (Ord a) => SetProblem a -> [Constraint a]
constraints (SetProblem goal _) = [Covered x | x <- S.toList goal]

satisfiers :: (Ord a) => SetProblem a -> Constraint a -> [Satisfier a]
satisfiers (SetProblem goal candidates) (Covered e) =
  map Include . S.toList . S.filter (S.member e) $ candidates

solveSet p = solve (constraints p) (satisfiers p) []
