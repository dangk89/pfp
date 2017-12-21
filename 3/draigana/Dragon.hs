module Dragon where
import Data.List (intersperse,intercalate,transpose,maximumBy,delete,elem,nub)
import Data.Ord (comparing)
import Control.Parallel
import Data.List (maximumBy)
import Control.Parallel.Strategies

import Minmax

data Side = L | R | T | B
  deriving (Eq, Show, Read)

type Move = (Side, Int)

data Player = Red | Blue
  deriving (Eq, Show, Read)

type Incomplete = (Int      -- size of the board
                  , [Move]  -- moves made so far
                  )

----------------------------------------------------------------

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


-- Takes row, checks row composition responds accordlingly: Push row if cell not empty else insert at cell
rowComp :: [Maybe Player] -> [Maybe Player]
rowComp row
    | not $ elem Nothing row = init row
    | otherwise = delete Nothing row


-- Apply a move to a board (Only from the left) uses rowcomp to insert dragon appropriately
apply :: Player -> Move -> Board -> Board
apply p (x,y) board = rowsBefore ++ (([Just p]++rowComp new) : rowsAfter) -- 
  where (rowsBefore, new : rowsAfter) = splitAt (y-1) board


-- Rotate board and apply moves to the rotated board using apply function from above
rotApply :: Player -> Move -> Board -> Board
rotApply p (L,y) board = apply p (L,y) board
rotApply p (R,y) board = map reverse (apply p (R,y) (map reverse board))
rotApply p (T,y) board = transpose (apply p (T,y) (transpose board))
rotApply p (B,y) board = reverse (transpose (apply p (B,y) (map reverse (transpose board))))

-- Check for winning lines
checkList :: [Maybe Player] -> Maybe Player
checkList row
    | nub row == [Just Red] = Just Red
    | nub row == [Just Blue] = Just Blue
    | otherwise = Nothing

-- Check horizontal lines for winning lines
linesHorizontal :: Board -> [Maybe Player]
linesHorizontal board = map checkList board

-- Check vertical for winning lines
linesVertical :: Board -> [Maybe Player]
linesVertical board = map checkList $ transpose board

-- If player has more lines than the other, return that player
lineWinner :: Board -> Maybe Player
lineWinner b =
    let c = (linesHorizontal b)++(linesVertical b)
        reds = length $ filter (==Just Red) c
        blues = length $ filter (==Just Blue) c
    in if blues < reds then Just Red else if blues > reds then Just Blue else Nothing

-- All possible moves
allMoves :: Int -> Board -> [Move]
allMoves n b = [ (x, y) | x <- [L,R,T,B], y <- [1..n]]

-- Calculates all possible confs from a given configuration (is given to the aimove or evaluate function
moves :: Int -> Conf -> [Conf]
moves n (mv,p, board)
    | lineWinner board /= Nothing = []
    | otherwise = [(mv, (toggle p), rotApply p mv board)::Conf | mv <- allMoves n board]

-- Creates board of size n from list of moves (Note that player should always be Red, as Red always starts)
mkBoard :: Int -> Player -> [Move] -> Board
mkBoard n p [] = emptyBoard n
mkBoard n p [x] = rotApply p x (emptyBoard n)
mkBoard n p (x:xs) = rotApply p x (mkBoard n (toggle p) xs)

-- Creates Conf from Incomplete
mkConf :: Incomplete -> Conf
mkConf (n, mvs) =
    let p = if (length mvs) `mod` 2 == 0 then Red else Blue
        b = mkBoard n Red mvs
        in ((L,1),p,b)


-- Heuristic for aimove function
static :: Player -> Conf -> Int
static me (_,_, board) = maybe 0 won $ lineWinner board
  where won winner = if me == winner then 1 else -1

-- Returns the best next move given Incomplete as input
nextMove :: Incomplete -> Move
nextMove (n,mvs) =
    let (mv,me,b) = mkConf (n,mvs)
        (m,_,_) = aimove 3 (moves n) (static me) (mv,me,b)
        in m


ms = [(L,2),(T,2),(L,2),(B,2),(R,2)]::[Move]
ms3 = [(L,2),(T,2),(L,2),(B,2),(R,2),(B,3)]::[Move]
ms2 = [(L,2),(L,3),(L,2),(L,3),(L,2),(L,4),(L,2),(L,3)]


board = mkBoard 4 Red ms
board2 = mkBoard 4 Red ms2
board3 = [[Just Blue,Just Red,Nothing,Nothing],[Just Blue,Just Blue,Nothing,Just Blue],[Nothing,Nothing,Nothing,Nothing],[Nothing,Just Red,Nothing,Nothing]]


-- Som lavet i nextmove hvis den tager Kens eksempel som incomplete input
cf1 = ((L,2), Blue, board)::Conf

bbb = [[Nothing,Just Blue,Nothing,Nothing],[Just Red,Just Red,Just Red,Just Red],[Nothing,Nothing,Nothing,Nothing],[Nothing,Just Blue,Just Blue,Nothing]]
inc = (4, ms)::Incomplete
cf2 = ((L,2), Blue, board3)::Conf

bb = [[Just Red,Just Blue,Nothing,Nothing],[Just Red,Just Red,Nothing,Just Red],[Nothing,Nothing,Nothing,Nothing],[Nothing,Just Blue,Nothing,Nothing]]

inc2 = (5, ms)::Incomplete

cfs = moves 4 cf1
