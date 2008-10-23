module ExactCover (solve) where

import Data.Set (Set,(\\))
import qualified Data.Set as S
import Debug.Trace

solve :: [c] -> (c -> [s]) -> [s] -> [[s]]
solve = undefined

data Problem constraint satisfier = Problem {
    constraints :: Set constraint,
    unknowns :: Set satisfier,
    knowns :: Set satisfier,
    constraintsF :: satisfier -> Set constraint,
    satisfiersF :: constraint -> Set satisfier
  }

makeEmptyProblem base c s =
  let bs = S.fromList base
      cs = S.fromList . c
      ss = S.fromList . s
      uns = S.unions (map ss base)
      kns = S.empty
  in Problem bs uns kns cs ss

nextProblem prob = do
  let cs = S.toList $ constraints prob
  let numSatisfiers cst = S.size . S.intersection (unknowns prob) $ (satisfiersF prob cst)
  let cs_ = map (\c -> (numSatisfiers c, c)) cs
  let (_,c) = minimum cs_
  -- this line acts as the only necessary guard in the whole algorithm.
  -- backtracking occurs if newKnown tries to pull from an empty list
  let possibleKnowns = S.toList . S.intersection (unknowns prob) . satisfiersF prob $ c
  newKnown <- possibleKnowns
  return $ move newKnown prob

move newKnown prob =
  let knowns_ = S.insert newKnown (knowns prob)
      allConstraintsRemoved = constraintsF prob newKnown
      constraintsRemoved = S.intersection (constraints prob) allConstraintsRemoved
      constraints_ = (constraints prob) \\ constraintsRemoved
      satisfiersRemoved = S.intersection (unknowns prob) (S.unions (map (satisfiersF prob) (S.toList constraintsRemoved)))
      unknowns_ = (unknowns prob) \\ satisfiersRemoved
      ret = Problem constraints_ unknowns_ knowns_ (constraintsF prob) (satisfiersF prob)
  in ret

solutions prob = do
  np <- nextProblem prob
  if (S.null . unknowns $ np) then return np else solutions np

solveFromConstraints cs c s = map (S.toList . knowns) . solutions $ (makeEmptyProblem cs c s)

solveWithSatisfiers satis c s cs =
  let 
    start = makeEmptyProblem cs c s
    middle = foldl (\p m -> move m p) start satis
  in map (S.toList . knowns) . solutions $ middle
