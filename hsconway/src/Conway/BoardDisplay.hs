module Conway.BoardDisplay
  where

import Conway.Board

displayBoard :: Board -> IO ()
displayBoard b = do
  horizRow
  mapM_ printRow (cells b)
  horizRow
  where printRow row = putStrLn $ "|" ++ concatMap show row ++ "|"
        horizRow = putStrLn $ "+" ++ replicate boardWidth '-' ++ "+"

