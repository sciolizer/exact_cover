module Pentominos where

import Array
import ExactCover
import ArrayDefault
import List (nubBy, sort)

data Board = Board (Array (Int,Int) Bool)
  deriving (Show)

data Constraint = 
    PentominoUsed Pentomino
  | SquareFilled (Int,Int)
  deriving (Eq,Show)

data Pentomino = I | N | L | U | X | W | P | F | Z | T | V | Y
  deriving (Enum,Bounded,Eq,Show)

boundedEnum :: (Enum a, Bounded a) => [a]
boundedEnum = enumFromTo minBound maxBound

data Rotation = Zero | Ninety | OneEighty | TwoSeventy
  deriving (Enum, Bounded, Eq, Show)

data Position = Position Rotation Bool -- bool for if it has been flipped
  deriving (Eq,Show)

instance Enum Position where
  fromEnum (Position rot False) = fromEnum rot
  fromEnum (Position rot True) = fromEnum (maxBound :: Rotation) + 1 + fromEnum rot
  toEnum n | n <= fromEnum (maxBound :: Rotation) = Position (toEnum n) False
           | otherwise = Position (toEnum (n - 1 - (fromEnum (maxBound :: Rotation)))) True
instance Bounded Position where
  minBound = Position Zero False
  maxBound = Position TwoSeventy True

data Move = Place Pentomino Position (Int,Int)
  deriving (Eq,Show)

constraints :: Board -> [Constraint]
constraints (Board arr) =
    [PentominoUsed p | p <- boundedEnum]
 ++ [SquareFilled c | (c,v) <- assocs arr, v ]

satisfiers :: Board -> Constraint -> [Move]
satisfiers b@(Board arr) cst =
  let validMoves = filter (validMove b) in nubBy equivMove . validMoves $
  case cst of
    PentominoUsed pent ->
      [Place pent pos coord | (coord,v) <- assocs arr, v, pos <- boundedEnum]
    SquareFilled coord ->
      [Place pent pos c | (c,v) <- assocs arr, v, pos <- boundedEnum, pent <- boundedEnum, coord `elem` (coords (Place pent pos c))]

equivMove :: Move -> Move -> Bool
equivMove m1 m2 = sort (coords m1) == sort (coords m2)

s2c :: Move -> [Constraint]
s2c move@(Place pent pos coord) = (PentominoUsed pent):[SquareFilled c | c <- coords move]

--check :: ConsistencyCheck Constraint Move
check = consistency_check (constraints boardWithHole) (satisfiers boardWithHole) s2c

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

partiallySolved :: [Move]
partiallySolved =
  [ Place I (Position Zero False) (0,0)
  , Place N (Position Zero False) (0,5)
  , Place L (Position Zero False) (0,6)
  , Place U (Position Zero False) (1,0)
  , Place X (Position Zero False) (2,2)
  --, Place W (Position Zero False) (1,3)
  --, Place P (Position Zero False) (4,0)
  ]

answer = case solve (constraints boardWithHole) (satisfiers boardWithHole) $ partiallySolved of
           Left err -> Left err
           Right foo -> Right $ head foo

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

solvePentomino :: Board -> Either (SolveError Constraint Move) [Move]
solvePentomino brd = case solve (constraints brd) (satisfiers brd) $ [] of
                       Left err -> Left err
                       Right foo -> Right $ head foo

sampleBoard = Board (array_default True ((0,0),(4,11)) [])

smallBoard = Board (array_default True ((0,0),(1,1)) [])

boardWithHole = Board (array_default True ((0,0),(7,7)) [((3,3),False),((3,4),False),((4,3),False),((4,4),False)])
