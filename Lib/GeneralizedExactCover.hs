module Lib.GeneralizedExactCover where

import ExactCover

data Constraint c
  = Required c
  | Optional c -- must be satisfied AT MOST once
  deriving (Eq,Ord,Show)

data Satisfier s c
  = Normal s
  | PlaceHolder c -- used in a solution when the associated optional constraint is met zero times
  deriving (Eq,Ord,Show)

solveGeneralized :: (Ord c, Ord s) => [c] -> [c] -> (c -> [s]) -> [s] -> [[s]]
solveGeneralized required optional satisfiers knowns = map unTag $ solve constraints' satisfiers' knowns' where
  constraints' = map Required required ++ map Optional optional
  satisfiers' (Required c) = map Normal . satisfiers $ c
  satisfiers' (Optional c) = (PlaceHolder c):(map Normal . satisfiers $ c)
  knowns' = map Normal knowns
  unTag = concatMap removePlaceHolder
  removePlaceHolder (Normal s) = [s]
  removePlaceHolder (PlaceHolder _) = []