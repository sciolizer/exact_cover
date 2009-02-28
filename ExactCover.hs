module ExactCover (solve, SolveError (InvalidMove)) where
--consistency_check
import List (nub, minimumBy, find)
import qualified List as L
import Maybe

import qualified Lib.TypedIntSet as IS
import Lib.TypedIntSet (TypedIntSet(TypedIntSet,unTypedIntSet))
import qualified Lib.TypedIntMap as IM
import Lib.TypedIntMap (Lib.TypedIntMap, (!))
import Lib.TypedInt

import Debug.Trace

data Problem c s = Problem {
  probConstraints :: TypedIntSet c,
  probUnknowns :: TypedIntSet s,
  probKnowns :: TypedIntSet s
  }

-- "known" and "satisfier" are very similar.
-- A "satisfier" meets a certain constraint. A "known" is a satisfier which
-- applies to the current problem.

data SolveError c s =
    InvalidMove s [s]
  deriving (Show)

bubbleError :: [Either (SolveError c s) a] -> Either (SolveError c s) [a]
bubbleError [] = Right []
bubbleError (Left err:xs) = Left err
bubbleError (Right x:xs) = case bubbleError xs of
                             Left err -> Left err
                             Right xs -> Right $ x : xs

collapseError :: Either (SolveError c s) [a] -> [Either (SolveError c s) a]
collapseError (Left err) = [Left err]
collapseError (Right xs) = [Right x | x <- xs ]

eitherBind :: (c -> Either b d) -> Either b c -> Either b d
eitherBind f (Left e) = Left e
eitherBind f (Right r) = case f r of { Left e -> Left e; Right x -> Right x }

typedInt :: [a] -> Int -> TypedInt a
typedInt _ = TypedInt

l !!! (TypedInt i) = l !! i

solve :: (Eq c, Eq s) => [c] -> (c -> [s]) -> [s] -> Either (SolveError c s) [[s]]
solve constraints_ satisfiers_ knowns_ =
  let
    -- revertS s = IM.findIndicesSet (== s)
    -- revertC c = IM.findIndicesSet (== c)
    revertS = map (satisfiersMap !) . IS.toList
    revertC = map (constraintsMap !) . IS.toList
    -- revertS is = map (fst . (satisfiersMap !!!)) . IS.toList $ is
    -- revertC is = map (fst . (constraintsMap !!!)) . IS.toList $ is
    constraintsMap = IM.fromList $ zip (map (typedInt constraints_) [0..]) constraints_ 
    satisfiersMap = IM.fromList $ zip (map (typedInt knowns_) [0..]) (nub . concatMap satisfiers_ $ constraints_)
    -- indexOf mp x = fromJust $ lookup x mp
    indexOf mp x = IM.findIndex (== x) mp
    -- c2s :: TypedIntMap c (TypedIntSet s)
    c2s = IM.fromList . map (\c -> (indexOf constraintsMap c, IS.fromList $ map (indexOf satisfiersMap) (satisfiers_ c))) $ constraints_
    -- s2c :: TypedIntMap s (TypedIntSet c)
    s2c = transpose c2s

    solutions prob@(Problem c u k) = eitherBind descend (children prob) where
      descend childs = bubbleError $ do
        child <- childs
        if (IS.null . probUnknowns $ child) then return (Right child) else collapseError $ solutions child

    children prob@(Problem c u k) = bubbleError $ do
      trace (show . unTypedIntSet $ k) (return ())
      -- deterministically pick the constraint with the least number of satisfiers
      let numSatisfiers = IS.size . IS.intersection u . (c2s !)
      let possibleKnowns = IS.intersection u . (c2s !) . minimumByMap numSatisfiers
      -- nondeterministically pick a satisfier for the chosen constraint
      newKnown <- IS.toList . possibleKnowns $ c
      return $ move prob newKnown

    move prob@(Problem c u k) newKnown =
      if not (IS.member newKnown u)
      then Left (InvalidMove (satisfiersMap ! newKnown) (revertS k))
      else
       Right $
        Problem
          (IS.difference c (s2c ! newKnown))
          (IS.difference u (IS.unions (map (c2s !) . IS.toList $ (s2c ! newKnown))))
          (IS.insert newKnown k)

    indices mp = IM.keysSet mp
    blank = Problem (indices constraintsMap) (indices satisfiersMap) IS.empty
    moveWithError (Left err) _ = Left err
    moveWithError (Right r) k = move r k
    withKnowns = foldl moveWithError (Right blank) (map (indexOf satisfiersMap) knowns_)
    asSatisfiers = map (satisfiersMap !) . IS.toList . probKnowns
  --in (Right $ mapM asSatisfiers) =<< solutions =<< withKnowns
  in case withKnowns of
       Left err -> Left err
       Right prob ->
         case solutions prob of
           Left err_ -> Left err_
           Right sls -> Right $ map asSatisfiers sls
    
minimumByMap :: (Ord b) => (TypedInt a -> b) -> TypedIntSet a -> TypedInt a
minimumByMap f = snd . minimumBy orderFst . map (\x -> (f x, x)) . IS.toList

orderFst :: (Ord a) => (a,b) -> (a,b) -> Ordering
orderFst (x,_) (y,_) = compare x y

transpose :: TypedIntMap a (TypedIntSet b) -> TypedIntMap b (TypedIntSet a)
transpose grid = IM.fromList [(k, refs k) | k <- gridVals] where
  gridVals = IS.toList . IS.unions . IM.elems $ grid
  refs k = IS.fromList . IM.keys . IM.filter (IS.member k) $ grid

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
