module Levels where

import qualified Data.ByteString as BS
import Debug.Trace (trace)
import Model
import Utility
import Xeno.DOM (parse)

loadLevels :: FilePath -> IO [Level]
loadLevels f = do
  files <- getAbsDirectoryContents f
  mapM loadLevel files

loadLevel :: FilePath -> IO Level
loadLevel f = do
  xml <- trace ("loadLevel: " ++ show f) BS.readFile f
  let (Right dom) = parse xml

  return $ Level "Test" [] []
