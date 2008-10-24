module ExactCover (solve) where

import List (nub, minimumBy)
import Maybe

import qualified Data.IntSet as IS
import Data.IntSet (IntSet)
import qualified Data.IntMap as IM
import Data.IntMap (IntMap, (!))

data Problem = Problem {
  probConstraints :: IntSet,
  probUnknowns :: IntSet,
  probKnowns :: IntSet
  }

-- "known" and "satisfier" are very similar.
-- A "satisfier" meets a certain constraint. A "known" is a satisfier which
-- applies to the current problem.

solve :: (Eq c, Eq s) => [c] -> (c -> [s]) -> [s] -> [[s]]
solve constraints_ satisfiers_ knowns_ =
  let
    constraintsMap = zip constraints_ [0..]
    satisfiersMap = zip (nub . concatMap satisfiers_ $ constraints_) [0..]
    indexOf mp x = fromJust $ lookup x mp
    c2s :: IntMap IntSet
    c2s = IM.fromList . map (\c -> (indexOf constraintsMap c, IS.fromList $ map (indexOf satisfiersMap) (satisfiers_ c))) $ constraints_
    s2c :: IntMap IntSet
    s2c = transpose c2s

    solutions prob@(Problem c u k) = do
      child <- children prob
      if (IS.null . probUnknowns $ child) then return child else solutions child

    children prob@(Problem c u k) = do
      -- deterministically pick the constraint with the least number of satisfiers
      let numSatisfiers = IS.size . IS.intersection u . (c2s !)
      let possibleKnowns = IS.intersection u . (c2s !) . minimumByMap numSatisfiers
      -- nondeterministically pick a satisfier for the chosen constraint
      newKnown <- IS.toList . possibleKnowns $ c
      return $ move prob newKnown

    move prob@(Problem c u k) newKnown =
      Problem
        (IS.difference c (s2c ! newKnown))
        (IS.difference u (IS.unions (map (c2s !) . IS.toList $ (s2c ! newKnown))))
        (IS.insert newKnown k)

    indices mp = IS.fromList (map snd mp)
    blank = Problem (indices constraintsMap) (indices satisfiersMap) IS.empty
    withKnowns = foldl move blank (map (indexOf satisfiersMap) knowns_)
    asSatisfiers = map (fst . (satisfiersMap !!)) . IS.toList . probKnowns
  in map asSatisfiers . solutions $ withKnowns
    
minimumByMap :: (Ord b) => (Int -> b) -> IntSet -> Int
minimumByMap f = snd . minimumBy (\(x,_) (y,_) -> compare x y) . map (\x -> (f x, x)) . IS.toList

transpose :: IntMap IntSet -> IntMap IntSet
transpose = undefined
