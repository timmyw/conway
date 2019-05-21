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
  
    -- * Display/string generation function
  , mkRowString
  , mkHorizRow
  )

  where

--import           Control.Monad
import           Data.Char
import           Conway.Board
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import           System.IO
import           System.FilePath

import Debug.Trace

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

data FileFormat = Conway | RLE | Json | Unknown

-- | Determine file format from filename
getFileFormat :: String -> FileFormat
getFileFormat filePath = case extension of
  ".conway" -> Conway
  ".rle" -> RLE
  ".json" -> Json
  _ -> Unknown
  where extension =  map toLower $ snd $ splitExtension filePath

-- | Save the supplied board to the specified file.  The format of the
-- output is determined by the extension in the file path.  Currently
-- supported formats are Conway (plain text), JSON and RLE.
saveBoard :: Board              -- ^ The board to be saved
          -> String             -- ^ The path to save the board to
          -> IO ()
saveBoard board filePath = case fileType of
  Conway -> saveBoardConway board filePath
  RLE -> saveBoardRLE board filePath
  Json -> saveBoardJson board filePath
  _ -> error $ "Unsupported file format for: " ++ filePath
  where fileType = getFileFormat filePath

-- | Save the board in RLE format
saveBoardRLE :: Board -> String -> IO ()
saveBoardRLE board filePath = do
  withFile filePath WriteMode (\h -> do
                                  hPutStrLn h $ "x = " ++ show (width board) ++ ", y = " ++ show (height board)
                                  mapM_ (\r -> hPutStr h $ genRow r) $ cells board
                                  hPutStrLn h "!"
                              )
    where
      genRow r = case (genRow' r) of
        (rowString, ch, count) -> if rowString == ""
                                  then
                                    show count ++ [conwayToRLE ch] ++ "$"
                                  else 
                                    rowString ++ "$"
      genRow' r = foldl buildRow ("", 99, 0) r


buildRow :: (String, Integer, Integer) -> Integer -> (String, Integer, Integer)
buildRow (build, curCh, count) ch =
  if ch == curCh
  then (build, curCh, count+1)
  else if curCh /= 99
       then (build ++ newChunk, ch, 1)
       else (build, ch, 0)
  where newChunk = show count ++ [conwayToRLE curCh]

-- | Translate a conway cell value to RLE code
conwayToRLE :: Integer -> Char
conwayToRLE 0 = 'b'
conwayToRLE 1 = 'o'
conwayToRLE _ = '*'

-- | Save the board in Conway format
saveBoardConway :: Board
                -> String
                -> IO ()
saveBoardConway board filePath = do
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

