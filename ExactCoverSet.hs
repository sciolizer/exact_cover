module Main where

import qualified Data.Set as S

data Constraint a = Covered a

data Problem a = Problem (Set a) (Set (Set a))

constraints :: Problem a -> Constraint a
constraints (Problem goal _) = [Covered x | x <- S.toList goal]

data Move a = Include (Set a)

satisfiers :: Problem a -> Constraint a -> [Move a]
satisfiers (Problem goal candidates) (Covered e) =
  map Include . S.filter (S.member e) $ candidates
