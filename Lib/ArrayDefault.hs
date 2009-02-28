module Lib.ArrayDefault where

import Ix
import Array

array_default :: (Ix i) => e -> (i, i) -> [(i,e)] -> Array i e
array_default def rng overrides =
  array rng ((zip (range rng) . repeat $ def) ++ overrides)
