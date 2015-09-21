{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO.Error
import Text.Read (readMaybe)

import Data.Char
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import Network.HTTP.Types
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import System.Console.CmdArgs.Explicit

import UnixSocket
import ModifiedTime

data Interface
    = InterfacePort Warp.Port
    | InterfaceUnixSocket FilePath

instance Show Interface where
    show (InterfacePort port) = "port " ++ show port
    show (InterfaceUnixSocket path) = "Unix socket " ++ path

arguments :: Mode [(Char, String)]
arguments = mode "plate" [] "Plate HTTP Server"
    (flagArg (upd 'c') "CONFIGFILE")
    [ flagReq ["socket","s"]
        (upd 's') "PORT_OR_PATH" "Port number or Unix domain socket path"
    , flagNone ["debug", "d"]
        (('d', "") :) "Run the server in the debug mode or not"
    , flagHelpSimple (('h', "") :)
    ]
  where
    upd msg x v = Right $ (msg, x) : v

main :: IO ()
main = do
    args <- processArgs arguments
    printHelpAndQuitOr args $ do
        let
            settings = Warp.setFdCacheDuration
                (if ('d', "") `elem` args then 0 else 60)
                Warp.defaultSettings
            interface = case lookup 's' args of
                Just s -> case readMaybe s :: Maybe Warp.Port of
                    Just p -> InterfacePort p
                    Nothing -> InterfaceUnixSocket s
                Nothing -> InterfacePort 3000
        putStrLn $ "Plate is opening at " ++ (show interface) ++ ".."
        case interface of
            InterfacePort p -> Warp.runSettings (Warp.setPort p settings) app
            InterfaceUnixSocket s -> do
                sock <- unixSocket s
                Warp.runSettingsSocket settings sock app
  where
    printHelpAndQuitOr args action = if ('h', "") `elem` args
        then print $ helpText [] HelpFormatDefault arguments
        else action

app :: Wai.Application
app req respond = do
    if isSafeURL $ Wai.rawPathInfo req
        then serveNormal req >>= respond
        else respond notFound

notFound :: Wai.Response
notFound = Wai.responseLBS status404
    [(hContentType, "text/plain")]
    "Page not found"

notModified :: Wai.Response
notModified = Wai.responseLBS status304 [] ""

serveNormal :: Wai.Request -> IO Wai.Response
serveNormal req
    | "/static/" `B.isPrefixOf` url = serveStatic
    | url == "/favicon.ico" = serve
        "image/vnd.microsoft.icon" "static/img/favicon.ico"
    | url == "/robots.txt" = serve
        "text/plain" "robots.txt"
    | url == "/" = serve htmlctype $ htmlpath "/index/"
    | "/" `B.isSuffixOf` url = serve htmlctype $ htmlpath url
    | otherwise = return notFound
  where
    url = Wai.rawPathInfo req
    htmlctype = "text/html; charset=utf-8"
    htmlpath path = "output" `mappend` B.init path `mappend` ".html"
    serveStatic
        | ".jpg"  `B.isSuffixOf` url = serveRelURL "image/jpeg"
        | ".png"  `B.isSuffixOf` url = serveRelURL "image/png"
        | ".svg"  `B.isSuffixOf` url = serveRelURL "image/svg+xml"
        | ".js"   `B.isSuffixOf` url = serveRelURL "application/javascript"
        | ".css"  `B.isSuffixOf` url = serveRelURL "text/css"
        | ".pdf"  `B.isSuffixOf` url = serveRelURL "application/pdf"
        | otherwise = return notFound
    serveRelURL ctype = serve ctype (B.drop 1 url)
    serve ctype path = ioMaybe (return notFound) (return . useMTime)
        (getMTime path)
      where
        useMTime mtime = case modifiedSince req mtime of
            NotModified -> notModified
            Modified -> Wai.responseFile status200
                [ (hContentType, ctype)
                , (hLastModified, formattedMTime mtime)
                ]
                (B.unpack path) Nothing

ioMaybe
    :: IO b         -- what to do on error
    -> (a -> IO b)  -- what to do on no error
    -> IO a         -- original action
    -> IO b
ioMaybe onError normally action = do
    tried <- tryIOError action
    case tried of
        Left _ -> onError
        Right v -> normally v

rtake :: Int -> ByteString -> ByteString
rtake n b = B.drop (B.length b - n) b

isSafeURL :: ByteString -> Bool
isSafeURL url = and
    [ B.all urlChar url
    , not $ "//" `B.isInfixOf` url
    , not $ "../" `B.isInfixOf` url
    , not $ ".." `B.isSuffixOf` url
    ]

urlChar :: Char -> Bool
urlChar c = or
    [ ord 'a' <= n && n <= ord 'z'
    , ord 'A' <= n && n <= ord 'Z'
    , ord '0' <= n && n <= ord '9'
    , c `B.elem` "-_./"
    ]
  where
    n = ord c
