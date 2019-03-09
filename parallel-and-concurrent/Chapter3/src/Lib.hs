module Lib where

import Control.Parallel.Strategies (using, parList, rpar)
import System.Posix (FileOffset, fileSize, getFileStatus)
import System.Directory (getDirectoryContents)

getFileSize :: FilePath -> IO FileOffset
getFileSize path = do
    stat <- getFileStatus path
    return (fileSize stat)

lsDir :: String -> IO [FilePath]
lsDir = getDirectoryContents

getFileSizes :: String -> IO [(String, FileOffset)]
getFileSizes path = do
    files <- getDirectoryContents path
    let paths = map (path ++) files
    sizes <- mapM getFileSize paths  -- `using` parList rpar
    return $ zip paths sizes
