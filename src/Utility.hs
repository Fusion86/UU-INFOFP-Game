module Utility where

import Data.List (isPrefixOf)
import System.Directory (canonicalizePath, getDirectoryContents)
import System.FilePath (joinPath, takeBaseName, (</>))

-- Taken from https://stackoverflow.com/a/8572250
getAbsDirectoryContents :: FilePath -> IO [FilePath]
getAbsDirectoryContents dir = do
  files <- getDirectoryContents dir
  let filteredFiles = filter ignoreDotFiles files
  mapM (canonicalizePath . (dir </>)) filteredFiles
  where
    -- This also ignores the "." and "..", which is what we want.
    ignoreDotFiles f = not ("." `isPrefixOf` f)
