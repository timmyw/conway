{-# LANGUAGE DeriveDataTypeable #-}
module Main
where

import Conway.Board
import Conway.BoardDisplay
import Data.Text as T
import System.Console.CmdArgs

data Options = Options {
  config :: String
  , starting :: Int
  , iterations :: Int
  }
  deriving (Show, Data, Typeable)
  
options = Options {
  config = def &= help "Specify a CONFIG file" &= typ "CONFIG"
  , starting = 50 &= help "COUNT of many cells are alive at start" &= typ "COUNT"
  , iterations = 100 &= help "COUNT of how many times to run" &= typ "COUNT"
  }
  &= program "conway"
  &= summary "Play Conway's game of life"
               
appName :: String
appName = "conway"

main :: IO ()
main = do
  opts <- cmdArgs options
  let its = iterations opts
  putStrLn $ "Running " ++ show its ++ " iterations"
  board <- createRandomBoard $ starting opts
  -- print (show board)
  displayBoard board
  -- print =<< cmdArgs (options)
  let board2 = boardIterate board
  displayBoard board2
