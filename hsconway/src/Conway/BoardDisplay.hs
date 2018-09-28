module Conway.BoardDisplay
  where

import Conway.Board

displayBoard :: Board -> IO ()
displayBoard b = do
  mapM_ printRow (cells b)
  where printRow row = putStrLn $ concat $ map show row 
