module Main where

data Matrix = Matrix (Array (Int,Int) Bool)

data Constraint = Column n deriving (Eq)

constraints :: Matrix -> [Constraint]
constraints (Matrix arr) =
  let (l_bound, u_bound) = bounds arr
  in [Column c | c <- [(snd l_bound)..(snd u_bound)]]

data Move = Row n

satisfiers :: Matrix -> Constraint -> [Move]
satisfiers (Matrix arr) (Column c) =
  let (l_bound, u_bound) = bounds arr
  in [Row r | r <- [(fst l_bound)..(fst u_bound)], arr ! (r, c)]
