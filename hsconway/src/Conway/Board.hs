module Conway.Board
  (
    Board(..)
    
  , createEmptyBoard
  , createRandomBoard

  , boardIterate
  
  , boardWidth
  , boardHeight
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

getCellNeighbourCount :: (Int, Int) -> Board -> Int
getCellNeighbourCount (x,y) board =
  undefined

createEmptyBoard :: Board
createEmptyBoard = Board { cells = replicate boardHeight mkRow }
  where mkRow = replicate boardWidth 0

createRandomBoard :: Int -> IO Board
createRandomBoard cnt = do
  g <- getStdGen
  let poss  = take (cnt*2) (randomRs (0, boardWidth-1) g)
  let poss' = drop cnt poss
  let pairs = zip (take cnt poss) (take cnt poss')
  let r = foldr setCell b pairs
  return r
  where b = createEmptyBoard 
        setCell  = setBoardCellValue 1

boardIterate :: Board  -> Board
boardIterate board =
  foldr (\(x,y) b -> processCell b x y) createEmptyBoard coords
  where coords = zip [0..boardHeight-1] [0..boardWidth-1]

processCell :: Board -> Int -> Int -> Board
processCell b x y = setBoardCellValue newValue (x, y) b
  where curValue = getBoardCellValue (x, y) b
        newValue = 0
        neightCount = getCellNeighbourCount (x,y) b
