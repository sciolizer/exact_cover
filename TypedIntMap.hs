module TypedIntMap where

import qualified Data.IntMap as IM
import qualified TypedIntSet as TIS
import TypedInt

data TypedIntMap key val = TypedIntMap { unTypedIntMap :: (IM.IntMap val) }

fromList = TypedIntMap . IM.fromList . map (\(TypedInt ti, b) -> (ti,b))

keys = map TypedInt . IM.keys . unTypedIntMap

elems = IM.elems . unTypedIntMap

filter f = TypedIntMap . IM.filter f . unTypedIntMap

keysSet = TIS.TypedIntSet . IM.keysSet . unTypedIntMap
