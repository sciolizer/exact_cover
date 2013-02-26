{-# LANGUAGE ScopedTypeVariables #-}
module ExactCover (solve) where

-- Knuth's Algorithm X, using the List monad for non-determinism

import Data.List (minimumBy)

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Set (Set)
import Data.Map (Map,(!))
import Data.Function (on)

data Problem c s = Problem {
  probConstraints :: Set c,
  probUnknowns :: Set s,
  probKnowns :: Set s
  }

solve :: forall c s. (Ord c, Ord s) => [c] -> (c -> [s]) -> [s] -> [[s]]
solve constraints_ satisfiers_ knowns_ =
  let
    c2s :: Map c (Set s)
    c2s = M.fromList . map (\c -> (c, S.fromList $ (satisfiers_ c))) $ constraints_

    s2c :: Map s (Set c)
    s2c = transpose c2s

    solutions :: Problem c s -> [Problem c s]
    solutions prob = do
      child <- children prob
      let isNull f = S.null (f child)
      if isNull probConstraints && isNull probUnknowns then return child else solutions child

    children :: Problem c s -> [Problem c s]
    children prob@(Problem c u k) = do
      -- deterministically pick the constraint with the least number of satisfiers
      let numSatisfiers = S.size . S.intersection u . (c2s !)
      let possibleKnowns = S.intersection u . (c2s !) . minimumBy (compare `on` numSatisfiers) . S.toList
      -- nondeterministically pick a satisfier for the chosen constraint
      newKnown <- S.toList . possibleKnowns $ c
      return $ move prob newKnown

    move :: Problem c s -> s -> Problem c s
    move prob@(Problem c u k) newKnown =
      if not (S.member newKnown u)
      then error "invalid state" -- two or more knowns_ met the same constraint
      else Problem
        -- remove all constraints met by chosen satisfier
        (S.difference c (s2c ! newKnown))
        -- remove all satisfiers of those constraints
        (S.difference u (S.unions (map (c2s !) . S.toList $ (s2c ! newKnown))))
        -- add chosen satisfier as a new known
        (S.insert newKnown k)

    blank = Problem (M.keysSet c2s) (M.keysSet s2c) S.empty

  in return . S.toList . probKnowns =<< solutions (foldl move blank knowns_)
    
transpose :: (Ord a, Ord b) => Map a (Set b) -> Map b (Set a)
transpose grid = M.fromList [(k, refs k) | k <- gridVals] where
  gridVals = S.toList . S.unions . M.elems $ grid
  refs k = S.fromList . M.keys . M.filter (S.member k) $ grid
