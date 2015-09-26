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

data Command
    = CmdCompile
    | CmdRun OpenAt Bool
    | CmdHelp

data OpenAt
    = OpenAtPort Warp.Port
    | OpenAtUnixSocket FilePath

parseOpenAt :: String -> OpenAt
parseOpenAt s = case readMaybe s :: Maybe Warp.Port of
    Just p  -> OpenAtPort p
    Nothing -> OpenAtUnixSocket s

instance Show OpenAt where
    show (OpenAtPort port) = "port " ++ show port
    show (OpenAtUnixSocket path) = "Unix socket " ++ path

arguments :: Mode Command
arguments = modes "plate" CmdHelp "Plate website serving tool"
    [compileCmd, runCmd]

compileCmd :: Mode Command
compileCmd = mode "compile" CmdHelp "apply templating and prepare serving"
    (flagArg (\ _ _ -> Right CmdCompile) "") []

runCmd :: Mode Command
runCmd = mode "run" CmdHelp "launch HTTP server"
    (flagArg (\ _ _ -> Right $ CmdRun (OpenAtPort 3000) False) "")
    [ flagReq ["socket", "s"]
        socketUpd "PORT_OR_PATH" "Port number or Unix domain socket path"
    , flagNone ["debug", "d"]
        debugUpd "Run the server in the debug mode or not"
    ]
  where
    socketUpd s (CmdRun _ d) = Right $ CmdRun (parseOpenAt s) d
    socketUpd _ x = Right x
    debugUpd (CmdRun s _) = CmdRun s True
    debugUpd x = x

main :: IO ()
main = do
    args <- processArgs arguments
    case args of
        CmdHelp -> print $ helpText [] HelpFormatDefault arguments
        CmdCompile -> undefined
        CmdRun openAt debug -> run openAt debug

run :: OpenAt -> Bool -> IO ()
run openAt debug = case openAt of
    OpenAtPort p -> Warp.runSettings (Warp.setPort p settings) app
    OpenAtUnixSocket s -> do
        sock <- unixSocket s
        Warp.runSettingsSocket settings sock app
  where
    settings = Warp.setFdCacheDuration (if debug then 0 else 60)
        Warp.defaultSettings

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
