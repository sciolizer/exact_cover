{-# LANGUAGE ScopedTypeVariables #-}
module ExactCover (solve, SolveError (InvalidMove)) where
--consistency_check
import List (nub, minimumBy, find)
import qualified List as L
import Maybe
import Control.Monad.Error
import Control.Monad.List

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Set (Set)
import Data.Map (Map,(!))
import Debug.Trace

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

data SolveError c s = InvalidMove s [s] deriving (Show)

instance Error (SolveError c s)

toT :: (Monad m) => [a] -> ListT m a
toT xs = msum . map return $ xs

solve :: forall c s. (Ord c, Ord s) => [c] -> (c -> [s]) -> [s] -> Either (SolveError c s) [[s]]
solve constraints_ satisfiers_ knowns_ =
  let
    c2s :: Map c (Set s)
    c2s = M.fromList . map (\c -> (c, S.fromList $ (satisfiers_ c))) $ constraints_
    s2c :: Map s (Set c)
    s2c = transpose c2s

    solutions :: Problem c s -> ListT (Either (SolveError c s)) (Problem c s)
    solutions prob = do
      child <- children prob
      if (S.null . probUnknowns $ child) then return child else solutions child

    children :: Problem c s -> ListT (Either (SolveError c s)) (Problem c s)
    children prob@(Problem c u k) = do
      -- deterministically pick the constraint with the least number of satisfiers
      let numSatisfiers = S.size . S.intersection u . (c2s !)
      let possibleKnowns = S.intersection u . (c2s !) . minimumByMap numSatisfiers
      -- nondeterministically pick a satisfier for the chosen constraint
      newKnown <- toT $ S.toList . possibleKnowns $ c
      lift $ move prob newKnown

    move :: Problem c s -> s -> Either (SolveError c s) (Problem c s)
    move prob@(Problem c u k) newKnown =
      
      if not (S.member newKnown u)
      then throwError (InvalidMove newKnown (S.toList k))
      else return $ Problem
        (S.difference c (s2c ! newKnown))
        (S.difference u (S.unions (map (c2s !) . S.toList $ (s2c ! newKnown))))
        (S.insert newKnown k)

    blank = Problem (M.keysSet c2s) (M.keysSet s2c) S.empty
    withKnowns :: Either (SolveError c s) (Problem c s)
    withKnowns = foldM move blank knowns_
    asSatisfiers = S.toList . probKnowns
  in runListT $ return . asSatisfiers =<< solutions =<< lift withKnowns
    
minimumByMap :: (Ord b) => (a -> b) -> Set a -> a
minimumByMap f = snd . minimumBy orderFst . map (\x -> (f x, x)) . S.toList

orderFst :: (Ord a) => (a,b) -> (a,b) -> Ordering
orderFst (x,_) (y,_) = compare x y

transpose :: (Ord a, Ord b) => Map a (Set b) -> Map b (Set a)
transpose grid = M.fromList [(k, refs k) | k <- gridVals] where
  gridVals = S.toList . S.unions . M.elems $ grid
  refs k = S.fromList . M.keys . M.filter (S.member k) $ grid

data ConsistencyError c s = KD (KeyDiff c s) | VD (ValDiff c s)
  deriving (Show)

data KeyDiff c s = KeyDiff {
  inTransposition :: [s],
  inProvision :: [s]
  }
  deriving (Show)

data ValDiff c s = ValDiff {
  valDiffKey :: s,
  inTranspositionVal :: [c],
  inProvisionVal :: [c]
  }
  deriving (Show)
  
{-
consistency_check :: (Eq c, Eq s) => [c] -> (c -> [s]) -> (s -> [c]) -> Maybe (ConsistencyError c s)
consistency_check base_constraints_ satisfiers_ constraints_ =
  let
    constraintsMap = zip base_constraints_ [0..]
    satisfiersMap = zip (nub . concatMap satisfiers_ $ base_constraints_) [0..]
    indexOf mp x = lookup x mp
    --c2s :: IntMap IntSet
    c2s = IM.fromList . concatMap (\c -> case indexOf constraintsMap c of { Nothing -> []; Just i -> [(i, IS.fromList $ concatMap (\s -> case indexOf satisfiersMap s of { Nothing -> []; Just i -> [i] }) (satisfiers_ c))] }) $ base_constraints_
    --s2c :: IntMap IntSet
    s2c = transpose c2s
    s2c_2 = IM.fromList . concatMap (\s -> case indexOf satisfiersMap s of { Nothing -> []; Just i -> [(i, IS.fromList $ concatMap (\c -> case indexOf constraintsMap c of { Nothing -> []; Just i -> [i] }) (constraints_ s))] }) $ (map fst satisfiersMap)
    keyInT = IS.difference (IM.keysSet s2c) (IM.keysSet s2c_2)
    keyInP = IS.difference (IM.keysSet s2c_2) (IM.keysSet s2c)
    revertS is = map (fst . (satisfiersMap !!) . unTypedInt) . IS.toList $ is
    revertC is = map (fst . (constraintsMap !!) . unTypedInt) . IS.toList $ is
    valInT k = IS.difference (s2c ! k) (s2c_2 ! k)
    valInP k = IS.difference (s2c_2 ! k) (s2c ! k)
  in
    if not (IS.null keyInT) || not (IS.null keyInP)
    then Just $ KD (KeyDiff (revertS keyInT) (revertS keyInP))
    else
      case find (\k -> not (IS.null (valInT k)) || not (IS.null (valInP k))) (IM.keys s2c) of
        Nothing -> Nothing
        Just k -> Just $ VD (ValDiff (fst $ satisfiersMap !! (unTypedInt k)) (revertC (valInT k)) (revertC (valInP k)))
  
-}
