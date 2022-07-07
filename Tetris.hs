-- | The Tetris game (main module)
module Main where

import ConsoleGUI         -- cabal install ansi-terminal
--import CodeWorldGUI     -- cabal install codeworld-api

-- local
import Shapes

--------------------------------------------------------------------------------
-- * The code that puts all the piece together

main = runGame tetrisGame

tetrisGame = Game { startGame     = startTetris,
                    stepGame      = stepTetris,
                    drawGame      = drawTetris,
                    gameInfo      = defaultGameInfo prop_Tetris,
                    tickDelay     = defaultDelay,
                    gameInvariant = prop_Tetris }

--------------------------------------------------------------------------------
-- * The various parts of the Tetris game implementation


-- | The state of the game
data Tetris = Tetris (Vector,Shape) Shape [Shape]
-- The state consists of three parts:
--   * The position and shape of the falling piece
--   * The well (the playing field), where the falling pieces pile up
--   * An infinite supply of random shapes

-- ** Positions and sizes

type Vector = (Int,Int)

-- | The size of the well
wellSize :: (Int,Int)
wellSize = (wellWidth,wellHeight)
wellWidth = 10
wellHeight = 20

-- | Starting position for falling pieces
startPosition :: Vector
startPosition = (wellWidth `div` 2 - 1, 0)

-- | Vector addition
vAdd :: Vector -> Vector -> Vector
(x1,y1) `vAdd` (x2,y2) = (x1+x2,y1+y2)

-- | Move the falling piece into position
place :: (Vector,Shape) -> Shape
place (v,s) = shiftShape v s


-- | An invariant that startTetris and stepTetris should uphold
prop_Tetris :: Tetris -> Bool
prop_Tetris (Tetris (vec, fallShape) well inf) = prop_Shape well && (shapeSize well == wellSize) -- TASK B04: complete me


-- | Add black walls around a shape
addWalls :: Shape -> Shape
addWalls s = S ([bRow] ++ rightBlck ++ [bRow]) -- TASK B05: complete me
  where
    (x,y) = shapeSize s
    bRow = replicate (x + 2) (Just Black)
    wider = padShape (1,0) (shiftShape (1,0) s)
    leftBlck = map (\x -> [Just Black] ++ tail x) (rows wider)
    rightBlck = map (\x -> reverse (tail (reverse x)) ++ [Just Black]) leftBlck

-- | Visualize the current game state. This is what the user will see
-- when playing the game.
drawTetris :: Tetris -> Shape
drawTetris (Tetris (v,p) w _) = addWalls (combine (shiftShape v p) w) -- TASK B06: complete me

move :: Vector -> Tetris -> Vector
move vCha (Tetris (v, p) w _) = vAdd vCha v

tick :: Tetris -> Maybe (Int, Tetris)
tick (Tetris (v, p) w inf) | collision tFuture = dropNewPiece (Tetris (v, p) w inf)
                           | otherwise = Just (0, tFuture)
  where
    v' = move (0, 1) (Tetris (v, p) w inf)
    tFuture = Tetris (v', p) w inf

collision :: Tetris -> Bool
collision (Tetris (v, p) w inf) = left || right || down || wellOverlap
  where
    (x, y) = v
    (width, h) = shapeSize p
    left = x < 0
    right = x + width > wellWidth
    down = y + h > wellHeight
    wellOverlap = (place (v, p)) `overlaps` w

movePiece :: Int -> Tetris -> Tetris
movePiece x (Tetris (v, p) w inf) | collision tNew = t
                                  | otherwise = tNew
  where
    t = (Tetris (v, p) w inf)
    vectNew = move (x, 0) t
    tNew = (Tetris (vectNew, p) w inf)

rotate :: Tetris -> Tetris
rotate (Tetris (v, p) w inf) = (Tetris (v, rotateShape p) w inf)

rotatePiece :: Tetris -> Tetris
rotatePiece (Tetris (v, p) w inf) | collision tNew = t
                                  | otherwise = tNew
  where
    t = Tetris (v, p) w inf
    tNew = rotate t

dropNewPiece :: Tetris -> Maybe (Int, Tetris)
dropNewPiece (Tetris (v, p) w (inf:infs)) | col = Nothing
                                          | otherwise = Just (n, t)
  where
    t = Tetris (startPosition, inf) shape' (infs)
    tOld = Tetris (startPosition, inf) shapeOldImpl (infs)
    shapeOldImpl = combine (place (v, p)) w
    col = overlaps inf shapeOldImpl
    (n, shape') = clearLines shapeOldImpl

clearLines :: Shape -> (Int, Shape)
clearLines (S rs) = (noRowsDestroyed,S newRs)
  where
    (width, height) = shapeSize (S rs)
    newRs = (replicate noRowsDestroyed (replicate width Nothing) ) ++ (filter (\x -> isComplete x == False) rs)
    noRowsDestroyed = foldr (\a b -> if isComplete a then 1 + b else b) 0 rs
    isComplete :: [Maybe Colour] -> Bool
    isComplete r = and (map (\x -> x /= Nothing) r)

-- | The initial game state
startTetris :: [Double] -> Tetris
startTetris rs = Tetris (startPosition,shape1) (emptyShape wellSize) supply
  where
    shape1:supply = [shapeSel x | x <- rs] -- TASK C08: complete me
    shapeSel :: Double -> Shape
    shapeSel num | (num * 7.0 > 0.0) && (num * 7.0 <= 1.0) = head (drop 0 allShapes)
                 | (num * 7.0 > 1.0) && (num * 7.0 <= 2.0) = head (drop 1 allShapes)
                 | (num * 7.0 > 2.0) && (num * 7.0 <= 3.0) = head (drop 2 allShapes)
                 | (num * 7.0 > 3.0) && (num * 7.0 <= 4.0) = head (drop 3 allShapes)
                 | (num * 7.0 > 4.0) && (num * 7.0 <= 5.0) = head (drop 4 allShapes)
                 | (num * 7.0 > 5.0) && (num * 7.0 <= 6.0) = head (drop 5 allShapes)
                 | (num * 7.0 > 6.0) && (num * 7.0 <= 7.0) = head (drop 6 allShapes)


-- | React to input. The function returns 'Nothing' when it's game over,
-- and @'Just' (n,t)@, when the game continues in a new state @t@.
stepTetris :: Action -> Tetris -> Maybe (Int,Tetris)
stepTetris Tick t = tick t
stepTetris MoveDown t = tick t
stepTetris MoveLeft t = Just (0, movePiece (-1) t)
stepTetris MoveRight t = Just (0, movePiece 1 t)
stepTetris Rotate t = Just (0, rotatePiece t)
--stepTetris _ t = Just (0,t)
