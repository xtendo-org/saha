module RecursiveContents
    ( getRecursiveContents
    ) where

import Control.Monad
import System.Directory
    (getDirectoryContents, doesDirectoryExist)
import System.FilePath ((</>))

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", ".."]) names
    paths <- forM properNames $ \name -> do
        let path = topdir </> name
        isDir <- doesDirectoryExist path
        if isDir
        then getRecursiveContents path
        else return [path]
    return (concat paths)

