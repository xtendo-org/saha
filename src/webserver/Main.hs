{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe
import System.IO.Error

import Data.Char
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import Network.HTTP.Types
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import System.Console.CmdArgs.Explicit

import UnixSocket
import ModifiedTime

c_UNIX_PATH :: String
c_UNIX_PATH = "/tmp/starplate.socket"

arguments :: Mode [(Char, String)]
arguments = mode "starplate" [] "Starplate HTTP Server"
    (flagArg (upd 'c') "CONFIGFILE")
    [ flagReq ["socket","s"]
        (upd 's') "PATH" "Unix domain socket path"
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
        let sockPath = fromMaybe c_UNIX_PATH (lookup 's' args)
        putStrLn $ "Starplate is opening at " ++ sockPath ++ " .."
        sock <- unixSocket sockPath
        if  ('d', "") `elem` args
            then Warp.runSettingsSocket
                (Warp.setFdCacheDuration 0 settings) sock app
            else Warp.runSettingsSocket settings sock app
  where
    settings = Warp.setPort 3000 $ Warp.defaultSettings
    printHelpAndQuitOr args action = if ('h', "") `elem` args
        then print $ helpText [] HelpFormatDefault arguments
        else action

app :: Wai.Application
app req respond = do
    if isSafeURL url
        then serveNormal req respond url
        else respond notFound
  where
    url = Wai.rawPathInfo req

notFound :: Wai.Response
notFound = Wai.responseLBS status404
    [(hContentType, "text/plain")]
    "Page not found"

notModified :: Wai.Response
notModified = Wai.responseLBS status304 [] ""

serveNormal
    :: Wai.Request
    -> (Wai.Response -> IO Wai.ResponseReceived)
    -> ByteString -> IO Wai.ResponseReceived
serveNormal req respond url
    | "/static/css/" `B.isPrefixOf` url = case rtake 4 url of
        ".css"  -> serve "text/css" $ B.drop 1 url
        _       -> respond notFound
    | "/static/img/" `B.isPrefixOf` url = case rtake 4 url of
        ".jpg"  -> serve "image/jpeg" $ B.drop 1 url
        ".png"  -> serve "image/png" $ B.drop 1 url
        ".svg"  -> serve "image/svg+xml" $ B.drop 1 url
        _       -> respond notFound
    | "/static/doc/" `B.isPrefixOf` url = case rtake 4 url of
        ".pdf"  -> serve "application/pdf" $ B.drop 1 url
        _       -> respond notFound
    | url == "/favicon.ico" = serve "image/vnd.microsoft.icon"
        "static/img/favicon.ico"
    | url == "/robots.txt" = serve "text/plain"
        "robots.txt"
    | url == "/" = serve htmlctype $ htmlpath "/index/"
    | "/" `B.isSuffixOf` url = serve htmlctype $ htmlpath url
    | otherwise = respond notFound
  where
    htmlctype = "text/html; charset=utf-8"
    htmlpath path = "output" `mappend` B.init path `mappend` ".html"
    serve ctype path = ioMaybe (respond notFound) useMTime (getMTime path)
      where
        useMTime mtime = case modifiedSince req mtime of
            NotModified -> respond notModified
            Modified -> respond $ Wai.responseFile status200
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
