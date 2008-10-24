module Main where

import Array

data Board = Board (Array (Int,Int) Bool)

data Constraint = 
    PentominoUsed Pentomino
  | SquareFilled (Int,Int)

data Pentomino = I | N | L | U | X | W | P | F | Z | T | V | Y
  deriving (Enum,Bounded)

boundedEnum :: (Enum a, Bounded a) => [a]
boundedEnum = enumFromTo minBound maxBound

data Rotation = Zero | Ninety | OneEighty | TwoSeventy
  deriving (Enum, Bounded)

data Position = Position Rotation Bool -- bool for if it has been flipped

instance Enum Position where
  fromEnum (Position rot False) = fromEnum rot
  fromEnum (Position rot True) = fromEnum (maxBound :: Rotation) + 1 + fromEnum rot
  toEnum n | n <= fromEnum (maxBound :: Rotation) = Position (toEnum n) False
           | otherwise = Position (toEnum (n - 1 - (fromEnum (maxBound :: Rotation)))) True
instance Bounded Position where
  minBound = Position Zero False
  maxBound = Position TwoSeventy True

data Move = Place Pentomino Position (Int,Int)

constraints :: Board -> [Constraint]
constraints (Board arr) =
    [PentominoUsed p | p <- boundedEnum]
 ++ [SquareFilled c | (c,v) <- assocs arr, v ]

satisfiers :: Board -> Constraint -> [Move]
satisfiers b@(Board arr) cst =
  let validMoves = filter (validMove b) in validMoves $
  case cst of
    PentominoUsed pent ->
      [Place pent pos coord | pos <- boundedEnum, coord <- indices arr]
    SquareFilled coord ->
      [Place pent pos coord | pos <- boundedEnum, pent <- boundedEnum]
  
validMoves b = filter (validMove b)

repr :: Pentomino -> [(Int,Int)]
repr I = [(0,0),(1,0),(2,0),(3,0),(4,0)]
repr N = [(0,0),(1,0),(1,1),(2,1),(3,1)]
repr L = [(0,0),(0,1),(1,1),(2,1),(3,1)]
repr U = [(0,0),(0,1),(1,0),(2,0),(2,1)]
repr X = [(0,0),(-1,0),(1,0),(0,-1),(0,1)]
repr W = [(0,0),(0,1),(1,1),(1,2),(2,2)]
repr P = [(0,0),(0,1),(0,2),(1,1),(1,2)]
repr F = [(0,0),(0,1),(0,2),(1,1),(-1,2)]
repr Z = [(0,0),(1,0),(1,1),(1,2),(2,2)]
repr T = [(0,0),(0,1),(0,2),(1,1),(2,1)]
repr V = [(0,0),(0,1),(0,2),(0,3),(-1,2)]
repr Y = [(0,0),(0,1),(0,2),(1,2),(2,2)]

-- There will of course be a little redundancy in coords, since some
-- of the pieces look identical when rotated.
coords :: Move -> [(Int,Int)]
coords (Place piece pos (row,col)) =
  map ((\(x,y) -> (x+row,y+col)) . applyPosition pos) . repr $ piece

validMove :: Board -> Move -> Bool
validMove (Board arr) m@(Place pent pos coord) = all (inRange (bounds arr)) (coords m)

flp :: Bool -> (Int,Int) -> (Int,Int)
flp False z = z
flp True (x,y) = (y,x)

rotate :: Rotation -> (Int,Int) -> (Int,Int)
rotate Zero z = z
rotate Ninety (x,y) = (-1*y, x)
rotate OneEighty (x,y) = (-1*x, -1*y)
rotate TwoSeventy (x,y) = (y, -1*x)

applyPosition :: Position -> (Int,Int) -> (Int,Int)
applyPosition (Position rot flpped) = flp flpped . rotate rot
