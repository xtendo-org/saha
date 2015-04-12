{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import System.Directory
import System.FilePath
import System.Posix.Files
import Data.List (isSuffixOf)

import Prelude hiding (readFile)
import Data.Text.IO (readFile, hPutStr)
import System.IO (withFile, IOMode(..))
import System.IO.Error

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
            targetPath = stDSTDIR ++ commonPath ++ ".html"
        mtime <- getMTime path
        fullText <- readFile path
        (headers, content) <- maybeAct (source fullText) $ do
            print path
            error "source file header parsing failed"
        let
            go = do
                createDirectoryIfMissing True $ stDSTDIR ++ dir
                writeOut mtime targetPath tpl headers content
        target <- tryIOError $ getMTime targetPath
        case target of
            Left _ -> go
            Right t -> if mtime == t then return () else go
  where
    getMTime = fmap modificationTime . getFileStatus
    noSuchHeader var = error $ "var " ++ show var ++ " not found in headers"
    isMd path = (".md" :: FilePath) `isSuffixOf` path
    maybeAct m a = maybe a return m
    writeOut mtime path tpl headers content = do
        putStrLn path
        withFile path WriteMode $ \ ohdl ->
            forM_ tpl $ \ seg -> hPutStr ohdl $ case seg of
                TextSegment t -> t
                Variable var -> if var == "content"
                    then commonmarkToHtml [] content
                    else maybe (noSuchHeader var) id (lookup var headers)
        setFileTimes path mtime mtime

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
