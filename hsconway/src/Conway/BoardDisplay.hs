{-|
Module      : Conway.BoardDisplay
Description : Board display
Copyright   : (c) Timmy Whelan, 2018-9
License     : BSD3
Maintainer  : tim@zipt.co
Stability   : provisional

Functions to display a board's current population state
-}
module Conway.BoardDisplay
  (
    -- * Board display functions
    displayBoard
  , displayBoards

  )

  where

import Conway.Board
import Conway.BoardIO

-- | Display the supplied board to stdout.
displayBoard :: Board            -- ^ The board to be displayed
             -> IO ()
displayBoard b = do
  horizRow
  mapM_ printRow (cells b)
  horizRow
  where printRow row = putStrLn $ mkRowString row
        horizRow = putStrLn mkHorizRow

-- | Display two boards side by side
displayBoards :: Board
              -> Board
              -> IO ()
displayBoards b1 b2 = do
  horizRow
  mapM_ printRows $ zip (cells b1) (cells b2)
  horizRow
  where
    printRows (r1, r2) = putStrLn $ (mkRowString r1) ++ separator ++ (mkRowString r2)
    horizRow = putStrLn mkHorizRow
    topRow =  "+" ++ replicate boardWidth '-' ++ "+"
    separator = "   --   "

