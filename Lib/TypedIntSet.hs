module Lib.TypedIntSet where

import qualified Data.IntSet as IS
import TypedInt

data TypedIntSet val = TypedIntSet { unTypedIntSet :: IS.IntSet }

toList = map TypedInt . IS.toList . unTypedIntSet

fromList = TypedIntSet . IS.fromList . map unTypedInt

null = IS.null . unTypedIntSet

size = IS.size . unTypedIntSet

intersection (TypedIntSet one) (TypedIntSet two) =
  TypedIntSet $ IS.intersection one two

difference (TypedIntSet one) (TypedIntSet two) =
  TypedIntSet $ IS.difference one two

member (TypedInt i) = IS.member i . unTypedIntSet

unions = TypedIntSet . IS.unions . map unTypedIntSet

insert (TypedInt i) = TypedIntSet . IS.insert i . unTypedIntSet

empty = TypedIntSet IS.empty
