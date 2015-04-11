{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import System.Directory
import System.FilePath
import Data.List (isSuffixOf)

import Prelude hiding (readFile)
import Data.Text.IO
import System.IO (withFile, IOMode(..))

import CMark

import Parser

stSRCDIR :: FilePath
stSRCDIR = "data"

stDSTDIR :: FilePath
stDSTDIR = "output"

stTPL_PATH :: FilePath
stTPL_PATH = "tpl/main.html"

main :: IO ()
main = do
    tpl <- fmap template $ readFile stTPL_PATH
    files <- getRecursiveContents stSRCDIR
    forM_ files $ \ path -> if not (isMd path) then return () else do
        let
            commonPath = dropExtension $ drop (length stSRCDIR) path
            dir = fst $ splitFileName commonPath
        fullText <- readFile path
        (headers, content) <- maybeAct (source fullText) $ do
            print path
            error "source file header parsing failed"
        createDirectoryIfMissing True $ stDSTDIR ++ dir
        withFile (stDSTDIR ++ commonPath ++ ".html") WriteMode $ \ ohdl ->
            forM_ tpl $ \ seg -> hPutStr ohdl $ case seg of
                TextSegment t -> t
                Variable var -> if var == "content"
                    then commonmarkToHtml [] content
                    else
                    maybe
                        (error $ "var " ++ show var ++ " not found in headers")
                        id
                        (lookup var headers)
  where
    isMd path = (".md" :: FilePath) `isSuffixOf` path
    maybeAct m a = maybe a return m

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", ".."]) names
    paths <- forM properNames $ \name -> do
        let path = topdir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then getRecursiveContents path
            else return [path]
    return (concat paths)
