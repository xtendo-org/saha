module Saha.Compile where

import Data.Maybe
import Control.Monad
import System.Directory (createDirectoryIfMissing)
import System.FilePath (dropExtension, splitFileName)
import System.Posix.Types (EpochTime)
import System.Posix.Files
import Data.List (isSuffixOf)

import Prelude hiding (readFile, writeFile)
import Data.Text (Text, unpack)
import Data.Text.IO (readFile, writeFile)
import System.IO.Error

import CMark

import Saha.Parser
import Saha.RecursiveContents

stSRCDIR :: FilePath
stSRCDIR = "data"

stDSTDIR :: FilePath
stDSTDIR = "output"

stTplPath :: FilePath
stTplPath = "tpl/"

data Conversion = Plaintext | CommonMark

compile :: IO ()
compile = do
    (mainTpl, mainTplMTime) <- getTemplate (stTplPath ++ "main.html")
    files <- getRecursiveContents stSRCDIR
    forM_ files $ \ path -> when (isMd path) $ do
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
            (tpl, tplMTime) <- case lookup "template" headers of
                Nothing -> return (mainTpl, mainTplMTime)
                Just v -> getTemplate (stTplPath ++ unpack v)
            let
                maxMTime = max mtime tplMTime
                doTheCopy = do
                    createDirectoryIfMissing True $ stDSTDIR ++ dir
                    writeOut (hPlaintext headers)
                        maxMTime targetPath tpl headers content
            targetMTime <- tryIOError $ getMTime targetPath
            case targetMTime of
                Left _ -> doTheCopy
                Right t -> when (t < maxMTime) doTheCopy
  where
    checkPublicity headers action = case lookup "publicity" headers of
        Nothing -> action
        Just v -> unless (v == "hidden") action
    isMd path = (".md" :: FilePath) `isSuffixOf` path
    maybeAct m a = maybe a return m
    hPlaintext headers = if ("plaintext", "plaintext") `elem` headers
        then Plaintext else CommonMark

getTemplate :: FilePath -> IO ([Template], EpochTime)
getTemplate path = do
    tpl <- template <$> readFile path
    mtime <- getMTime path
    return (tpl, mtime)

writeOut
    :: Conversion -> EpochTime -> FilePath -> [Template] -> [(Text, Text)]
    -> Text
    -> IO ()
writeOut conv mtime path tpl headers content = do
    putStrLn path
    writeFile path $ mconcat $ map segConvert tpl
    setFileTimes path mtime mtime
  where
    segConvert seg = case seg of
        TextSegment t -> t
        Variable var -> if var == "content"
            then case conv of
                CommonMark -> commonmarkToHtml [optSmart] content
                Plaintext -> content
            else fromMaybe (noSuchHeader var) $ lookup var headers
    noSuchHeader var = error $ "var " ++ show var ++ " not found in headers"

getMTime :: FilePath -> IO EpochTime
getMTime = fmap modificationTime . getFileStatus
