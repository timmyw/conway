{-|
Module      : Conway.Board
Description : Board storage and manipulation
Copyright   : (c) Timmy Whelan, 2018-9
License     : BSD3
Maintainer  : tim@zipt.co
Stability   : provisional

Provides storage types for a Conway board, with accessors.  Also
provides the iteration and rule application functions.
-}
module Conway.Board
  (
    -- * Types
    
    Board(..)
  , BoardRow(..)
  
    -- * Board generation
  , createEmptyBoard
  , createRandomBoard

    -- * Conway Iteration
  , boardIterate
  , determineCellFuture

  -- * Utility functions
  , getCellNeighbourCount
  , getBoardCellValue
  , setBoardCellValue

  -- * Constants
  , boardWidth
  , boardHeight
  )
  
  where

import           Data.List
import           System.Random

import Debug.Trace

-- | The width of any generation or handled boards
boardWidth :: Int
boardWidth = 20

-- | The height of any generation or handled boards
boardHeight :: Int
boardHeight = 20

-- | Stores a single row of the Board
type BoardRow = [Integer]

{- |
A board is a list of list of ints.  Each int stores the state of the
cell.
-}
newtype Board = Board {
                   cells :: [BoardRow]
                   }

showRow :: BoardRow -> String
showRow = concatMap show

instance Show Board where
  show b = intercalate "\n" $ map showRow (cells b)

-- | Retrieve the rth row of a board
getBoardRow :: Int -> Board -> BoardRow
getBoardRow r b = cells b !! r

-- | Set the rth row of a board
setBoardRow :: Int -> BoardRow -> Board -> Board
setBoardRow r newRow b =
  if r >= 0 && r <= boardHeight
  then 
    Board { cells = take r rows ++ [newRow] ++ drop (r+1) rows }
  else
    error $ "Row " ++ show r ++ " out of range"
  where rows = cells b

getRowCellValue :: Int -> BoardRow -> Integer
getRowCellValue x row = if x >=0 && x < boardWidth
                        then row !! x
                        else error $ "Width " ++ show x ++ " too large"

setRowCellValue :: Int -> Integer -> BoardRow -> BoardRow
setRowCellValue x newVal row =
  if x >= 0 && x <= boardWidth
  then
    take x row ++ [newVal] ++ drop (x+1) row
  else error $ "Width " ++ show x ++ " too large"

getBoardCellValue :: (Int, Int) -> Board -> Integer
getBoardCellValue (x,y) b = getRowCellValue x (getBoardRow y b)

setBoardCellValue :: Integer -> (Int, Int) -> Board -> Board
setBoardCellValue val (x,y) b = setBoardRow y newRow b
  where newRow = setRowCellValue x val (getBoardRow y b)

getCellNeighbourCount :: (Int, Int) -> Board -> Integer
getCellNeighbourCount (x,y) board = rowAbove + leftOne y + rightOne y + rowBelow
  where rowAbove = if y == 0
                   then 0
                   else leftOne (y-1) + getBoardCellValue (x, y-1) board + rightOne (y-1)
        rowBelow = if y == boardWidth-1
                   then 0
                   else leftOne (y+1) + getBoardCellValue (x, y+1) board + rightOne (y+1)
        leftOne r = if x == 0 then 0 else getBoardCellValue (x-1, r) board
        rightOne r = if x == boardWidth-1 then 0 else getBoardCellValue (x+1, r) board

-- | Create an empty board (full sized but all cell statuses are
-- 'empty')
createEmptyBoard :: Board
createEmptyBoard = Board { cells = replicate boardHeight mkRow }
  where mkRow = replicate boardWidth 0

-- | Creates a randomly generated board.  Works by generating a list
-- of random coords, and sets those to be populated.
createRandomBoard :: Int         -- ^ The number of cells to populate
                  -> IO Board    -- ^ The generated board
createRandomBoard cnt = do
  g <- getStdGen
  let poss  = take (cnt*2) (randomRs (0, boardWidth-1) g)
  let poss' = drop cnt poss
  let pairs = zip (take cnt poss) (take cnt poss')
  let r = foldr setCell b pairs
  return r
  where b = createEmptyBoard 
        setCell  = setBoardCellValue 1

-- | Runs an interation over the supplied board.  Each cell is
-- evaluated and has the Conway rules applied to it
boardIterate :: Board            -- ^ The starting state of the board
             -> Board            -- ^ The board after one complete iteration
boardIterate board = 
  foldr processCell' createEmptyBoard coords
  where coords = createBoardCoords
        processCell' (x,y) b = processCell b board x y

createBoardCoords :: [(Int, Int)]
createBoardCoords = Data.List.foldl (\acc y -> acc ++ makeRow y) [] [0..boardHeight-1]
  where
    makeRow y = Data.List.foldl (\acc x -> acc ++ [(x,y)])  [] [0..boardWidth-1]

-- | Processes a single cell.
processCell :: Board             -- ^ The new board
            -> Board             -- ^ The original board
            -> Int
            -> Int
            -> Board
processCell buildBoard currentBoard x y = setBoardCellValue newValue (x, y) buildBoard
  where curValue   = getBoardCellValue (x, y) currentBoard
        newValue   = determineCellFuture curValue neighbourCount
        neighbourCount = getCellNeighbourCount (x,y) currentBoard

determineCellFuture' :: Integer   -- ^ Current status
                     -> Integer   -- ^ Neighbour count
                     -> Integer   -- ^ New status
determineCellFuture' cur cnt = trace ("determine: " ++ show cur ++ " " ++ show cnt ++ " " ++ show (determineCellFuture cur cnt)) determineCellFuture cur cnt
  
determineCellFuture :: Integer   -- ^ Current status
                    -> Integer   -- ^ Neighbour count
                    -> Integer   -- ^ New status
determineCellFuture 1 0 = 0
determineCellFuture 1 1 = 0
determineCellFuture 1 2 = 1
determineCellFuture 1 3 = 1
determineCellFuture 1 4 = 0
determineCellFuture 1 5 = 0
determineCellFuture 1 6 = 0
determineCellFuture 1 7 = 0
determineCellFuture 1 8 = 0
determineCellFuture 0 0 = 0
determineCellFuture 0 1 = 0
determineCellFuture 0 2 = 0
determineCellFuture 0 3 = 1
determineCellFuture 0 4 = 0
determineCellFuture 0 5 = 0
determineCellFuture 0 6 = 0
determineCellFuture 0 7 = 0
determineCellFuture 0 8 = 0

