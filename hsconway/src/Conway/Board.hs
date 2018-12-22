module Conway.Board
  (
    Board(..)
    
  , createEmptyBoard
  , createRandomBoard
  )
  
  where

import Data.List
import System.Random

boardWidth :: Int
boardWidth = 20

boardHeight :: Int
boardHeight = 20

{-
A board is a list of list of ints.  Each int stores the state of the
cell.
-}

type BoardRow = [Integer]

data Board = Board {
                   cells :: [BoardRow]
                   }

showRow :: BoardRow -> String
showRow r = concat $ map (\c -> show c) r

instance Show Board where
  show b = intercalate "\n" $ map showRow (cells b)

-- | Retrieve the rth row of a board
getBoardRow :: Int -> Board -> BoardRow
getBoardRow r b = (cells b) !! r

-- | Set the rth row of a board
setBoardRow :: Int -> BoardRow -> Board -> Board
setBoardRow r newRow b =
  if r >= 0 && r <= boardHeight
  then 
    Board { cells = (take r rows) ++ [newRow] ++ (drop (r+1) rows) }
  else
    error $ "Row " ++ (show r) ++ " out of range"
  where rows = cells b

getRowCellValue :: Int -> BoardRow -> Integer
getRowCellValue x row = if x >=0 && x < boardWidth
                        then row !! x
                        else error $ "Width " ++ (show x) ++ " too large"

setRowCellValue :: Int -> Integer -> BoardRow -> BoardRow
setRowCellValue x newVal row =
  if x >= 0 && x <= boardWidth
  then
    (take x row) ++ [newVal] ++ (drop (x+1) row)
  else error $ "Width " ++ (show x) ++ " too large"

getBoardCellValue :: (Int, Int) -> Board -> Integer
getBoardCellValue (x,y) b = getRowCellValue x (getBoardRow y b)

setBoardCellValue :: (Int, Int) -> Integer -> Board -> Board
setBoardCellValue (x,y) val b = setBoardRow y newRow b
  where newRow = setRowCellValue x val (getBoardRow y b)
  
createEmptyBoard :: Board
createEmptyBoard = Board { cells = replicate (boardHeight+1) mkRow }
  where mkRow = replicate (boardWidth+1) 0

createRandomBoard :: Int -> IO Board
createRandomBoard cnt = do
  g <- getStdGen
  let pairs = zip (take cnt (randomRs (1, boardWidth) g)) (take cnt (randomRs (1, boardHeight) g))
  let r = foldr setCell b pairs
  return r
  where b = createEmptyBoard 
        setCell coords board = setBoardCellValue coords 1 board

          
