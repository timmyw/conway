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
    toJSON (Board board) =
        object ["board" .= object
                [ "width" .= boardWidth, "height" .= boardHeight]
               , "version" .= boardLayoutVersion
               , "rows" .= board
               ]


mkRowString :: BoardRow -> String
mkRowString row = "|" ++ concatMap tr row ++ "|"
  where tr 0 = " "
        tr 1 = "+"

mkHorizRow :: String
mkHorizRow = "+" ++ replicate boardWidth '-' ++ "+"

-- | Save the supplied board to the specified file
saveBoard :: Board
          -> String
          -> IO ()
saveBoard board filePath = do
  withFile filePath WriteMode (\h -> do
                                  hPutStrLn h mkHorizRow
                                  mapM_ (\r -> writeRow h r) $ cells board
                                  hPutStrLn h mkHorizRow
                              )
    where writeRow h r = hPutStrLn h $ mkRowString r

loadBoard :: String -> IO Board
loadBoard = undefined

-- | Save the board in JSON format to the specified filename
saveBoardJson :: Board               -- ^ The board to save
              -> String              -- ^ File path to save the board to
              -> IO ()
saveBoardJson board filePath = do
  writeFile filePath $ BS.unpack $ encode board

