module Lib.TypedIntMap where

import qualified Data.IntMap as IM
import qualified Lib.TypedIntSet as TIS
import Lib.TypedInt

data TypedIntMap key val = TypedIntMap { unTypedIntMap :: (IM.IntMap val) }

fromList = TypedIntMap . IM.fromList . map (\(TypedInt ti, b) -> (ti,b))

keys = map TypedInt . IM.keys . unTypedIntMap

elems = IM.elems . unTypedIntMap

filter f = TypedIntMap . IM.filter f . unTypedIntMap

keysSet = TIS.TypedIntSet . IM.keysSet . unTypedIntMap

(TypedIntMap mp) ! (TypedInt i) = case IM.lookup i mp of
                                    Nothing -> error ("key not found: " ++ show i)
                                    Just x -> x

findIndices :: (Eq v) => (v -> Bool) -> TypedIntMap k v -> [TypedInt k]
findIndices f mp = findIndices' f (IM.toList $ unTypedIntMap mp) where
  findIndices' f [] = []
  findIndices' f ((k,v):xs) | f v = (TypedInt k) : (findIndices' f xs)
                            | otherwise = findIndices' f xs

findIndex :: (Eq v) => (v -> Bool) -> TypedIntMap k v -> TypedInt k
findIndex f = head . findIndices f

findIndicesSet :: (Eq v) => (v -> Bool) -> TypedIntMap k v -> TIS.TypedIntSet k
findIndicesSet f = TIS.fromList . findIndices f
