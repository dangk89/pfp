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

data Cell = Player | Empty
          deriving (Eq, Show)

----------------------------------------------------------------


-- Takes row, checks row composition responds accordlingly (empty cells in the row, full rows)
rowComp :: [Maybe Player] -> Board-> [Maybe Player]
rowComp x b
    | not $ elem Nothing x = [Just (nextPlayer b)]++init x
    | otherwise = [Just (nextPlayer b)]++(delete Nothing x)

-- Apply a move to a board (Only from the left) (uses rowComp to check if cell is empty)
apply :: Move -> Board -> Board
apply (x,y) board = rowsBefore ++ ((rowComp new board) : rowsAfter) -- 
  where (rowsBefore, new : rowsAfter) = splitAt (y-1) board

-- Rotate board and apply moves to the rotated board
rotApply :: Move -> Board -> Board
rotApply (L,y) board = apply (L,y) board
rotApply (R,y) board = map reverse (apply (R,y) (map reverse board))
rotApply (T,y) board = transpose (apply (T,y) (transpose board))
rotApply (B,y) board = reverse (transpose (apply (B,y) (map reverse (transpose board))))

-- Make board from list of moves
makeBoard :: [Move] -> Board
makeBoard [x] = rotApply x (emptyBoard 4)
makeBoard (x:xs) = rotApply x (makeBoard xs)

-- All possible moves ## GØR DYNAMISK length af n i stedet for length af board
allMoves :: Board -> [Move]
allMoves b = [ (x, y) | x <- [L,R,T,B], y <- [1..length b]]

-- Create all possible boards from a board
--moves :: Board -> [Board]
--moves b = [(rot_apply p b) | p <- allMoves b]

moves :: Conf -> [Conf]
moves (_,p, board) = [(mv, p, rotApply mv board)::Conf | mv <- allMoves board]


--moves (mv,p, board)   
--    | hasWinner b1 /= Nothing = []
--    | otherwise = [(mv, p, rotApply mv board)::Conf | mv <- allMoves board]


-- Derives next player from Board
nextPlayer :: Board -> Player
nextPlayer board = if countMoves board `mod` 2 == 0 then Red else Blue
countMoves board =
  sum $ map (\ cell -> case cell of Nothing -> 0; _ -> 1) $ concat board

----------------------------------------------------------------

checkList :: [Maybe Player] -> Maybe Player
checkList row
    | nub row == [Just Red] = Just Red
    | nub row == [Just Blue] = Just Blue
    | otherwise = Nothing

winHorizontal :: Board -> Maybe Player
winHorizontal board = msum $ map checkList board

winVertical :: Board -> Maybe Player
winVertical board = msum $ map checkList $ transpose board

--winConf :: Conf -> Maybe Player
--winConf c =

hasWinner :: Board -> Maybe Player
hasWinner b = msum $ [winHorizontal, winVertical] <*> pure b


static :: Player -> Conf -> Int
static me (_,_, board) = maybe 0 won $ hasWinner board
  where won winner = if me == winner then 1 else -1


----------------------------------------------------------------

-- Læser spil config
readIncomplete :: IO Incomplete
readIncomplete = do
  inp <- getContents
  let ls     = lines inp
      n      = read $ head ls
      moves  = parseMoves $ tail ls
  return (n, moves)

-- Oversætter liste til liste af moves
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

-- Dummy implementation, just repeats the last move or starts with T2
nextMove :: Incomplete -> Move
nextMove (n, mvs) =
    let b = makeBoard mvs
        p = nextPlayer b
        (mv,pl,m) = aimove 1 moves (static p) c1
        in mv

--------------------------------------------------------------------------------


move1 = (L,1)::Move
inc1 = (4,[move1,move1])::Incomplete  
board1 = [map Just [Red,Blue]]::Board

b1 = [[Just Blue, Just Red,Nothing],[Nothing,Nothing,Nothing],[Nothing,Nothing,Just Red]]::Board
ms1 = [(L,4),(L,2),(L,4),(R,2),(L,4),(L,2),(L,4),(L,1)]::[Move]
ms11 = [(L,1),(L,4),(L,1),(L,4),(L,1),(R,4)]

tb = [[Just Blue, Nothing, Nothing, Just Red],[Nothing,Just Blue,Nothing,Nothing],[Nothing,Nothing,Just Blue,Nothing],[Nothing,Nothing,Nothing,Just Blue]]::Board

ms2 = [(L,2),(T,2),(L,2),(B,2),(R,2)]::[Move]

ms3 = [(L,1),(L,2),(L,1),(L,3),(L,1),(L,2),(L,1),(L,2),(L,4),(L,2)]::[Move]

c1 = ((L,2), Red, (makeBoard ms11))::Conf
c2 = ((L,2), Blue, (makeBoard ms2))::Conf

inc = (4,[(L,2),(T,2),(L,2),(B,2),(R,2)])::Incomplete


-- TO DO
-- CONFCACHE, CONFWIN
-- MOVE I CONF, HOW???
    -- evt brug Nothing og Maybe Move??

mvz = [(L,1),(L,1),(L,1),(B,4),(B,4),(B,4)]::[Move]


b3 = [[Nothing,Just Red,Nothing,Nothing],[Just Blue,Just Blue,Nothing,Just Blue],[Nothing,Nothing,Nothing,Nothing],[Nothing,Just Red,Nothing,Just Red]]





board = makeBoard ms2




ams = allMoves board
bs = [rotApply mv board | mv <- ams]
abc = [static Red ((L,1),Blue,e) | e <- bs]
tbb = rotApply (L,2) board
tbbb = rotApply (R,2) tbb