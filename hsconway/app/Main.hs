{-# LANGUAGE DeriveDataTypeable #-}

module Main
where

import Conway.Board
import Conway.BoardIO
import Conway.BoardDisplay
import System.Console.ANSI
import System.Console.CmdArgs
import Control.Concurrent
import Data.Time

data Options = Options {
  config :: String
  , starting :: Int
  , iterations :: Int
  , delay :: Int
  , boardFile :: String
  , initWidth :: Int
  , initHeight :: Int
  , convertFile :: String
  }
  deriving (Show, Data, Typeable)

options :: Options
options = Options {
  config = def &= help "Specify a CONFIG file" &= typ "CONFIG"
  , starting = 50 &= help "COUNT of many cells are alive at start" &= typ "COUNT"
  , iterations = 100 &= help "COUNT of how many times to run" &= typ "COUNT"
  , delay = 1000 &= help "COUNT of miliseconds between iterations" &= typ "COUNT"
  , boardFile = "" &= help "FILE to load instead of a random board" &= typ "FILE"
  , initWidth = 20 &= help "COUNT width of the new board" &= typ "COUNT"
  , initHeight = 20 &= help "COUNT height of the new board" &= typ "COUNT"
  , convertFile = "" &= help "FILE to write out board FILE to" &= typ "FILE"
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
  
doIterations :: Board -> Int -> Options -> IO ()
doIterations startBoard iterCount opts =
  doIterations' startBoard 0
  where
    doIterations' board cur =
          if cur > iterCount
          then
            return ()
          else
            do
              showTitle cur
              let board2 = boardIterate board
              displayBoards board board2
              Control.Concurrent.threadDelay (getDelay opts)
              doIterations' board2  (cur + 1)
    showTitle cur = do
      prepScreen
      setSGR [SetColor Foreground Vivid Red]
      putStrLn $ "Iteration " ++ show cur
      setSGR [Reset]
      setSGR [SetColor Foreground Vivid Blue]

getOutputFilename :: IO String
getOutputFilename = do
  t <- getZonedTime
--  return $ formatTime defaultTimeLocale "%Y%m%d%H%S.conway" t
  return $ formatTime defaultTimeLocale "001.conway" t
  
main :: IO ()
main = do
  prepScreen
  opts <- cmdArgs options
  let its = iterations opts
  setTitle $ "conway: Running " ++ show its ++ " iterations"
  putStrLn $ "Running " ++ show its ++ " iterations"
  board <- if boardFile opts == ""
          then do outputPath <- getOutputFilename
                  b <- createRandomBoard (initWidth opts)
                      (initHeight opts)
                      (starting opts)
                  saveBoard b outputPath
                  return b
          else do putStrLn $ "Loading board from " ++ boardFile opts
                  loadBoard $ boardFile opts
  let outFile = convertFile opts
  if outFile /= ""
    then
    saveBoard board outFile
    else do
      displayBoard board
      Control.Concurrent.threadDelay 10000
      doIterations board its opts

main' :: IO ()
main' = do
  board <- loadBoard "patterns/blinker.conway"
  displayBoard board
  let c = map (\x -> getCellNeighbourCount (x, 1) board) [0..(width board)-1]
  putStrLn $ "neighcount:" ++ show c
  let (x,y) = (4,2)
  putStrLn $ "coords:" ++ show (x,y)
  putStrLn $ "cur status:" ++ show (getBoardCellValue (x,y) board)
  putStrLn $ "neighcount:" ++ show (getCellNeighbourCount (x,y) board)
  putStrLn $ "new status:" ++ show (determineCellFuture (getBoardCellValue (x,y) board)  (getCellNeighbourCount (x,y) board))

  let board2 = setBoardCellValue (determineCellFuture (getBoardCellValue (x,y) board)  (getCellNeighbourCount (x,y) board))
        (x,y)
        board
  displayBoard board2
