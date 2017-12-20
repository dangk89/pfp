{-
  Code for tic-tac-toe games

  Date: Dec, 2017
  Author: Ken Friis Larsen <kflarsen@diku.dk>, Maya Saietz <mayasaietz@gmail.com>
-}

module TicTacToe where

import Control.Monad (msum)
import Data.List (nub,transpose)
import Minmax(aimove)

import Debug.Trace

data Player = Cross | Nought
            deriving (Eq, Show)

data Cell = Move Player | Empty
          deriving (Eq, Show)

isEmpty :: Cell -> Bool
isEmpty Empty = True
isEmpty _     = False

type Row = [Cell]
type Board = [Row]

emptyBoard :: Board
emptyBoard = take 3 emptyRows
  where emptyRows = repeat emptyRow
        emptyRow = take 3 (repeat Empty)

type Position = (Int, Int)

nextPlayer :: Board -> Player
nextPlayer board = if countMoves board `mod` 2 == 0 then Cross else Nought
countMoves board =
  sum $ map (\ cell -> case cell of Move _ -> 1; _ -> 0) $ concat board


-- You should probably take a good look at this function until you
-- understand it. Keep in mind that it does not check whether the move
-- is valid.
move :: Position -> Board -> Board
move (x, y) board = rowsBefore ++ (changed : rowsAfter)
  where (rowsBefore, toBeChanged : rowsAfter) = splitAt x board
        changed = cellsBefore ++ (newCell : cellsAfter)
        (cellsBefore, olde : cellsAfter) = splitAt y toBeChanged
        player = nextPlayer board
        newCell = Move player

getCell :: Board -> Position -> Cell
getCell board (x, y) = board !! x !! y

validMove :: Board -> Position -> Bool
validMove board pos = isEmpty (board `getCell` pos)

allMoves :: [Position]
allMoves = [ (x, y) | x <- [0 .. 2], y <- [0 .. 2] ]

allValidMoves :: Board -> [Position]
allValidMoves board = filter (validMove board) allMoves

checkList :: [Cell] -> Maybe Player
checkList cells = case nub cells of
                     [Move p] -> Just p
                     _        -> Nothing

winHorizontal :: Board -> Maybe Player
winHorizontal board = msum $ map checkList board

winVertical :: Board -> Maybe Player
winVertical board = msum $ map checkList $ transpose board

diagonal xs = zipWith (!!) xs [0..]

winDiagonal :: Board -> Maybe Player
winDiagonal board =
    msum $ map checkList [diagonal board, diagonal $ map reverse board]



hasWinner :: Board -> Maybe Player
hasWinner b = msum $ [winHorizontal, winVertical, winDiagonal] <*> pure b

-- The aimove from Minmax take two functions as argument:
--    - moves :: (a -> [a])
--      that for a given configuration finds all possible following
--      configuration
--
--    - static :: a -> Int
--      that make a rough estimate of the value of a position without
--      looking ahead.
--
-- For tic-tac-toe the configuration could just be the board, because
-- given two board we can deduce which move took us from one
-- configuration to the next. Also, from a board we can trivially
-- count who's turn is next.
-- However, to ease the programming of extracting the move made by the
-- AI, we'll let a configuration be a pair of a board and the move
-- that took us there. But what should our initial configuration then
-- be?  It doesn't matter as long as the `moves` and `static` ignore
-- that part of the configuration, the move is just there to make it
-- easier to use the result of aimove.

type Conf = (Position, Board)

moves :: Conf -> [Conf]
moves (_, board) = [ (p, move p board) | p <- allValidMoves board]


static :: Player -> Conf -> Int
static me (_, board) = maybe 0 won $ hasWinner boardf
  where won winner = if me == winner then 1 else -1


-- Simple text setup used for play test
showBoard board = mapM_ showRow board
  where showRow row = mapM_ showCell row >> putStrLn ""
        showCell (Move Cross)  = putStr "X"
        showCell (Move Nought) = putStr "O"
        showCell _             = putStr " "

type Brain = Board -> IO Position

human :: Brain
human board = do
  putStr "Your move: "
  readLn

computer :: Int -> Player -> Brain
computer depth player board = do
  let (pos, _) = aimove depth moves (static player) ((0,0), board)
  putStrLn $ "AI says: "++show pos
  return pos

useBrain brain board = do
  pos <- brain board
  return $ move pos board

play board brain1 brain2 = do
  showBoard board
  case (hasWinner board, any isEmpty $ concat board) of
    (Just p, _) -> do putStrLn $ (show p) ++ " has won"
    (_, False)  -> do putStrLn "It's a tie"
    _ -> do
      newBoard <- useBrain brain1 board
      play newBoard brain2 brain1

-- Example usages:
--
-- - Play two AI against each other
--     play emptyBoard (computer 3 Cross) (computer 7 Nought
--
-- - Play human against AI
--     play emptyBoard human (computer 7 Nought)
--
