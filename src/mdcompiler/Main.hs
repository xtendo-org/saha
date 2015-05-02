{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import System.Directory
import System.FilePath
import System.Posix.Files
import Data.List (isSuffixOf)

import Prelude hiding (readFile)
import Data.Text (unpack)
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
stTPL_PATH = "tpl/"

main :: IO ()
main = do
    mainTpl <- fmap template $ readFile $ stTPL_PATH ++ "main.html"
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
        checkPublicity headers $ do
            let
                getTpl = case (lookup "template" headers) of
                    Nothing -> return mainTpl
                    Just v -> fmap template $
                        readFile $ stTPL_PATH ++ unpack v
                go = do
                    tpl <- getTpl
                    createDirectoryIfMissing True $ stDSTDIR ++ dir
                    writeOut mtime targetPath tpl headers content
            target <- tryIOError $ getMTime targetPath
            case target of
                Left _ -> go
                Right t -> if mtime == t then return () else go
  where
    checkPublicity headers action = case lookup "publicity" headers of
        Nothing -> action
        Just v -> if v /= "hidden" then action else return ()
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
                    then commonmarkToHtml [optSmart] content
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
