{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Conway.BoardIO
Description : Board IO
Copyright   : (c) Timmy Whelan, 2018-9
License     : BSD3
Maintainer  : tim@zipt.co
Stability   : provisional

Functions to display a board's current population state
-}
module Conway.BoardIO
  (

    -- * Load/save the board
    loadBoard
  , saveBoard
  , saveBoardJson
  
    -- * Display/string generation function
  , mkRowString
  , mkHorizRow
  )

  where

--import           Control.Monad
import           Conway.Board
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import           System.IO

-- | Board structure/layout version
boardLayoutVersion :: Int
boardLayoutVersion = 2

-- | Dump out the supplied board in JSON
instance ToJSON Board where
    -- this generates a Value
    toJSON (Board { cells = board, width = width, height = height }) =
        object ["board" .= object
                [ "width" .= width, "height" .= height]
               , "version" .= boardLayoutVersion
               , "rows" .= board
               ]


mkRowString :: BoardRow -> String
mkRowString row = "|" ++ concatMap tr row ++ "|"
  where tr 0 = " "
        tr 1 = "+"
        tr x = take 1 $ show x

mkHorizRow :: Int -> String
mkHorizRow w = "+" ++ replicate w '-' ++ "+"

-- | Save the supplied board to the specified file
saveBoard :: Board
          -> String
          -> IO ()
saveBoard board filePath = do
  withFile filePath WriteMode (\h -> do
                                  hPrint h (width board)
                                  hPrint h (height board)
                                  hPutStrLn h $ mkHorizRow (width board)
                                  mapM_ (\r -> writeRow h r) $ cells board
                                  hPutStrLn h $ mkHorizRow (width board)
                              )
    where writeRow h r = hPutStrLn h $ mkRowString r

getLines :: FilePath -> IO [String]
getLines = fmap lines . readFile

loadBoard :: String -> IO Board
loadBoard filePath = do
  allRows <- getLines filePath
  let width' = (read (head allRows)) :: Int
  let height' = (read (allRows !! 1)) :: Int
  let rows = map dropBorders $ init $ drop 3 allRows
  return $ foldl (\acc (y,r) -> processRow y acc r width') (createEmptyBoard width' height') $ zip [0..width'-1] rows
  where
    dropBorders :: String -> String
    dropBorders r = init $ drop 1 r
    processRow :: Int -> Board -> String -> Int -> Board
    processRow y board r w = foldl (\acc (x,v) -> setBoardCellValue (trCell v) (x,y) acc) board $ zip [0..w-1] r
    trCell '+' = 1
    trCell ' ' = 0

-- | Save the board in JSON format to the specified filename
saveBoardJson :: Board               -- ^ The board to save
              -> String              -- ^ File path to save the board to
              -> IO ()
saveBoardJson board filePath =
  writeFile filePath $ BS.unpack $ encode board

