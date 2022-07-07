-- | Types and functions for shapes. The list of all tetris pieces.
module Shapes where

-- base
import Data.List  (transpose)
import Data.Maybe (isNothing)

-- QuickCheck
import Test.QuickCheck

-- * Shapes

type Square = Maybe Colour

data Colour = Black | Red | Green | Yellow | Blue | Purple | Cyan | Grey
              deriving (Eq,Bounded,Enum,Show)

-- | A geometric shape is represented as a list of lists of squares. Each square
-- can be empty or filled with a block of a specific colour.

data Shape = S [Row] deriving (Eq)
type Row = [Square]

rows :: Shape -> [Row]
rows (S rs) = rs

-- * Showing shapes

showShape :: Shape -> String
showShape s = unlines [showRow r | r <- rows s]
  where
    showRow :: Row -> String
    showRow r = [showSquare s | s <- r]

    showSquare Nothing = '.'
    showSquare (Just Black) = '#' -- can change to '█' on linux/mac
    showSquare (Just Grey)  = 'g' -- can change to '▓'
    showSquare (Just c)     = head (show c)

instance Show Shape where
  show = showShape
  showList ss r = unlines (map show ss)++r


-- * The shapes used in the Tetris game

-- | All 7 tetrominoes (all combinations of connected 4 blocks),
-- see <https://en.wikipedia.org/wiki/Tetromino>
allShapes :: [Shape]
allShapes = [S (makeSquares s) | s <- shapes]
   where
      makeSquares = map (map colour)
      colour c    = lookup c [('I',Red),('J',Grey),('T',Blue),('O',Yellow),
                              ('Z',Cyan),('L',Green),('S',Purple)]
      shapes =
              [["I",
               "I",
               "I",
               "I"],
              [" J",
               " J",
               "JJ"],
              [" T",
               "TT",
               " T"],
              ["OO",
               "OO"],
              [" Z",
               "ZZ",
               "Z "],
              ["LL",
               " L",
               " L"],
              ["S ",
               "SS",
               " S"]]

-- * Some simple functions

-- ** TASK A01
emptyShape :: (Int,Int) -> Shape
emptyShape (width, height) =  S (replicate height (replicate width Nothing))

-- ** TASK A02

-- | The size (width and height) of a shape
shapeSize :: Shape -> (Int,Int)
shapeSize (S rs) = (length (head rs), length rs)

-- ** TASK A03

-- | Count how many non-empty squares a shape contains
blockCount :: Shape -> Int
blockCount (S rs) = foldr (+) 0 (((map (\x -> if x == Nothing then 0 else 1)) . concat) rs)

-- * The Shape invariant

-- ** TASK A04
-- | Shape invariant (shapes have at least one row, at least one column,
-- and are rectangular)
prop_Shape :: Shape -> Bool
prop_Shape (S []) = False
prop_Shape (S rs) | all (\x -> length x >= 1) rs == False = False
                  | otherwise = all (\x -> x == length (head rs))(map length rs)

-- * Test data generators

-- ** TASK A05
-- | A random generator for colours
rColour :: Gen Colour
rColour = oneof [return Black, return Red, return Green, return Yellow,
  return Blue, return Purple, return Cyan, return Grey]


instance Arbitrary Colour where
  arbitrary = rColour

-- ** TASK A06
-- | A random generator for shapes
rShape :: Gen Shape
rShape = oneof [return y | y <- allShapes]


instance Arbitrary Shape where
  arbitrary = rShape

-- * Transforming shapes

-- ** TASK A07
-- | Rotate a shape 90 degrees
rotateShape :: Shape -> Shape
rotateShape (S rs) = S ((reverse . transpose) rs)

-- ** TASK A08
-- | shiftShape adds empty squares above and to the left of the shape
shiftShape :: (Int,Int) -> Shape -> Shape
shiftShape (hor, vert) (S rs)= shiftHor hor (shiftVert vert (S rs))

shiftVert :: Int -> Shape -> Shape
shiftVert vert (S rs) = S ((replicate vert (replicate (length (head rs)) Nothing)) ++ rs)

shiftHor :: Int -> Shape -> Shape
shiftHor hor (S rs) = S (transpose ((rows (shiftVert hor (S (transpose rs))))))

-- ** TASK A09
-- | padShape adds empty sqaure below and to the right of the shape
padShape :: (Int,Int) -> Shape -> Shape
padShape x s = (rotateShape . rotateShape) (shiftShape x ((rotateShape . rotateShape) s))

-- ** TASK A10
-- | pad a shape to a given size
padShapeTo :: (Int,Int) -> Shape -> Shape
padShapeTo (hor, ver) s = padShape (hor', ver') s
  where
    (x, y) = shapeSize s
    hor' = max (hor - x) 0
    ver' = max (ver - y) 0

-- * Comparing and combining shapes

-- ** TASK B01

-- | Test if two shapes overlap
overlaps :: Shape -> Shape -> Bool
s1 `overlaps` s2 = or (zipWith rowsOverlap (rows s1') (rows s2'))
  where
    (x1, y1) = shapeSize s1
    (x2, y2) = shapeSize s2
    (hor, ver) = (max x1 x2, max y1 y2)
    (s1', s2') = (padShapeTo (hor, ver) s1, padShapeTo (hor, ver) s2)

rowsOverlap :: Row -> Row -> Bool
rowsOverlap [] [] = False
rowsOverlap (Nothing:xs) (y:ys) = False || rowsOverlap xs ys
rowsOverlap (x:xs) (Nothing:ys) = False || rowsOverlap xs ys
rowsOverlap (x:xs) (y:ys) = True

-- ** TASK B02
-- | zipShapeWith, like 'zipWith' for lists
zipShapeWith :: (Square -> Square -> Square) -> Shape -> Shape -> Shape
zipShapeWith f (S r1) (S r2) = S (zipWith (\x y -> zipWith f x y) r1 r2)

-- ** TASK B03
-- | Combine two shapes. The two shapes should not overlap.
-- The resulting shape will be big enough to fit both shapes.
combine :: Shape -> Shape -> Shape
s1 `combine` s2 = zipShapeWith f s1' s2'
  where
    (x1, y1) = shapeSize s1
    (x2, y2) = shapeSize s2
    (hor, ver) = (max x1 x2, max y1 y2)
    (s1', s2') = (padShapeTo (hor, ver) s1, padShapeTo (hor, ver) s2)
    f :: Square -> Square -> Square
    f Nothing x = x
    f y Nothing = y
