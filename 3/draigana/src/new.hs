module Dragon where
import Data.List (intersperse,intercalate,transpose,maximumBy,delete,elem,nub)
import Data.Ord (comparing)

import Control.Monad (msum)
import Minmax

data Side = L | R | T | B
  deriving (Eq, Show, Read)

type Move = (Side, Int)

data Player = Red | Blue
  deriving (Eq, Show, Read)

type Incomplete = (Int      -- size of the board
                  , [Move]  -- moves made so far
                  )



-- Takes row, checks row composition responds accordlingly push row if cell not empty else insert
rowComp :: [Maybe Player] -> Board-> [Maybe Player]
rowComp x b
    | not $ elem Nothing x = init x
    | otherwise = delete Nothing x


-- Apply a move to a board (Only from the left) uses rowcomp to insert dragon appropriately
insertPlayer :: Player -> Move -> Board -> Board
insertPlayer p (x,y) board = rowsBefore ++ (p++(rowComp new board) : rowsAfter) -- 
  where (rowsBefore, new : rowsAfter) = splitAt (y-1) board

-- Rotate board and apply moves to the rotated board
insertRot :: Player -> Move -> Board -> Board
rotApply p (L,y) board = insertPlayer (L,y) board
rotApply p (R,y) board = map reverse (insertPlayer (R,y) (map reverse board))
rotApply p (T,y) board = transpose (insertPlayer (T,y) (transpose board))
rotApply p (B,y) board = reverse (transpose (insertPlayer (B,y) (map reverse (transpose board))))



-- Make board from list of moves
makeBoard :: Player -> [Move] -> Board
makeBoard p [x] = rotApply p x (emptyBoard 4)
makeBoard p (x:xs) = rotApply p x (makeBoard (toggle p) xs)


-- All possible moves = Always the same
allMoves :: Board -> [Move]
allMoves b = [ (x, y) | x <- [L,R,T,B], y <- [1..n]]













































-- Read game input
readIncomplete :: IO Incomplete
readIncomplete = do
  inp <- getContents
  let ls     = lines inp
      n      = read $ head ls
      moves  = parseMoves $ tail ls
  return (n, moves)


-- Vector of strings to vector of moves
parseMoves :: [String] -> [Move]
parseMoves ls = map parseMove ls
  where parseMove (s : idx) = (read [s], read idx)
        parseMove _ = error "Illegal move syntax"

printMove :: Move -> IO()
printMove (s, idx) =
  putStrLn $ show s ++ show idx


-- Representing boards with lists
type Board = [[Maybe Player]]
type Conf  = (Move, Player, Board)

-- LAV CACHE FÆRDIG
--confCache :: [Move] -> [Conf]
--confCache ms = [(nextPlayer b, b) | b <- makeBoard ms]

emptyBoard n = replicate n $ replicate n Nothing

showBoard :: Int -> Board -> String
showBoard n board = border ++ inner ++ border
  where
    border = "  " ++ (concat $ replicate n "+-") ++ "+\n"
    inner = intercalate border (map showLine board)
    showLine row = "  |" ++ (row >>= showPlayer) ++ "\n"
    showPlayer (Just Red) = "R|"
    showPlayer (Just _)   = "B|"
    showPlayer _          = " |"

toggle Red  = Blue
toggle Blue = Red


