{-# LANGUAGE ScopedTypeVariables #-}
module ExactCover (solve) where

import List (nub, minimumBy, find)
import Maybe
import Control.Monad

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Set (Set)
import Data.Map (Map,(!))
import Debug.Trace
import Data.Function

data Problem c s = Problem {
  probConstraints :: Set c,
  probUnknowns :: Set s,
  probKnowns :: Set s
  }

-- "known" and "satisfier" are very similar.
-- A "satisfier" meets a certain constraint. A "known" is a satisfier which
-- applies to the current problem.
--
-- throughout the code, the type variable 'c' is short for 'constraint',
-- and 's' is short for 'satisfier'

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
      if (S.null . probUnknowns $ child) then return child else solutions child

    children :: Problem c s -> [Problem c s]
    children prob@(Problem c u k) = do
      -- deterministically pick the constraint with the least number of satisfiers
      let numSatisfiers = S.size . S.intersection u . (c2s !)
      let possibleKnowns = S.intersection u . (c2s !) . minimumByMap numSatisfiers
      -- nondeterministically pick a satisfier for the chosen constraint
      newKnown <- S.toList . possibleKnowns $ c
      return $ move prob newKnown

    move :: Problem c s -> s -> Problem c s
    move prob@(Problem c u k) newKnown =
      if not (S.member newKnown u)
      then error "invalid state"
      else Problem
        (S.difference c (s2c ! newKnown))
        (S.difference u (S.unions (map (c2s !) . S.toList $ (s2c ! newKnown))))
        (S.insert newKnown k)

    blank = Problem (M.keysSet c2s) (M.keysSet s2c) S.empty
    withKnowns :: Problem c s
    withKnowns = foldl move blank knowns_
    asSatisfiers = S.toList . probKnowns
  in return . asSatisfiers =<< solutions withKnowns
    
minimumByMap :: (Ord b) => (a -> b) -> Set a -> a
minimumByMap f = snd . minimumBy (compare `on` fst) . map (\x -> (f x, x)) . S.toList

transpose :: (Ord a, Ord b) => Map a (Set b) -> Map b (Set a)
transpose grid = M.fromList [(k, refs k) | k <- gridVals] where
  gridVals = S.toList . S.unions . M.elems $ grid
  refs k = S.fromList . M.keys . M.filter (S.member k) $ grid
