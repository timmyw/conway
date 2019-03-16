{-# LANGUAGE DeriveDataTypeable #-}

module Main
where

import Conway.Board
import Conway.BoardDisplay
import Data.Text as T
import System.Console.ANSI
import System.Console.CmdArgs
import Control.Concurrent

data Options = Options {
  config :: String
  , starting :: Int
  , iterations :: Int
  , delay :: Int
  }
  deriving (Show, Data, Typeable)
  
options = Options {
  config = def &= help "Specify a CONFIG file" &= typ "CONFIG"
  , starting = 50 &= help "COUNT of many cells are alive at start" &= typ "COUNT"
  , iterations = 100 &= help "COUNT of how many times to run" &= typ "COUNT"
  , delay = 1000 &= help "COUNT of miliseconds between iterations" &= typ "COUNT"
  }
  &= program "conway"
  &= summary "Play Conway's game of life"
               
appName :: String
appName = "conway"

getDelay :: Options -> Int
getDelay opts = 1000 * delay opts

prepScreen :: IO ()
prepScreen = do
  clearScreen
  setCursorPosition 0 0
  
doIterations :: Board -> Board -> Int -> Options -> IO ()
doIterations origBoard board count opts =
  doIterations' board 0
  where
    doIterations' board cur =
          if cur > count
          then
            return ()
          else
            do
              showTitle cur
              let board2 = boardIterate board
              displayBoard board2
              Control.Concurrent.threadDelay (getDelay opts)
              doIterations' board2  (cur + 1)
    showTitle cur = do
      prepScreen
      setSGR [SetColor Foreground Vivid Red]
      putStrLn $ "Iteration " ++ show cur
      setSGR [Reset]
      setSGR [SetColor Foreground Vivid Blue]

main :: IO ()
main = do
  prepScreen
  opts <- cmdArgs options
  let its = iterations opts
  setTitle $ "conway: Running " ++ show its ++ " iterations"
  putStrLn $ "Running " ++ show its ++ " iterations"
  board <- createRandomBoard $ starting opts
  displayBoard board
  Control.Concurrent.threadDelay 1000000
  -- print =<< cmdArgs (options)
  doIterations board board its opts
