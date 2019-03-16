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
  where

import Conway.Board

-- | Display the supplied board to stdout.
displayBoard :: Board            -- ^ The board to be displayed
             -> IO ()
displayBoard b = do
  horizRow
  mapM_ printRow (cells b)
  horizRow
  where printRow row = putStrLn $ "|" ++ concatMap show row ++ "|"
        horizRow = putStrLn $ "+" ++ replicate boardWidth '-' ++ "+"

