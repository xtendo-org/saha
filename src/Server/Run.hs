module Server.Run
    ( run
    , OpenAt(..)
    , parseOpenAt
    ) where

import Prelude hiding ((++))

import Data.Char
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy as LB (fromStrict)

import System.IO.Error
import Text.Read (readMaybe)
import System.Posix.Files.ByteString (fileExist)

import Network.HTTP.Types
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import Server.UnixSocket
import Server.ModifiedTime

(++) :: Monoid m => m -> m -> m
(++) = mappend

-- OpenAt configuration

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

-- logic

run :: OpenAt -> Bool -> ByteString -> IO ()
run openAt debug absoluteHost = do
    B.putStrLn $ mconcat ["Saha is opening at ", B.pack (show openAt), ".."]
    case openAt of
        OpenAtPort p -> Warp.runSettings (Warp.setPort p settings) appWithHost
        OpenAtUnixSocket s -> do
            sock <- unixSocket s
            Warp.runSettingsSocket settings sock appWithHost
  where
    appWithHost = app absoluteHost
    settings = Warp.setFdCacheDuration (if debug then 0 else 60)
        Warp.defaultSettings

app :: ByteString -> Wai.Application
app absoluteHost req respond =
    if isSafeURL $ Wai.rawPathInfo req
    then serveNormal absoluteHost req >>= respond
    else respond notFound

serveNormal :: ByteString -> Wai.Request -> IO Wai.Response
serveNormal host req
    | "/static/" `B.isPrefixOf` url = serveStatic
    | url == "/favicon.ico" = serve
        "image/vnd.microsoft.icon" "static/img/favicon.ico"
    | url == "/robots.txt" = serve
        "text/plain" "robots.txt"
    | B.length url /= 1 && B.last url == '/' = checkRedirect
    | otherwise = serve htmlctype htmlpath
  where
    url = Wai.rawPathInfo req
    htmlctype = "text/html; charset=utf-8"
    htmlpath = if B.length url == 1
        then "output/index.html"
        else mconcat ["output", url, ".html"]
    serveStatic
        | ".jpg"    `B.isSuffixOf` url = serveRelURL "image/jpeg"
        | ".png"    `B.isSuffixOf` url = serveRelURL "image/png"
        | ".svg"    `B.isSuffixOf` url = serveRelURL "image/svg+xml"
        | ".js"     `B.isSuffixOf` url = serveRelURL "application/javascript"
        | ".css"    `B.isSuffixOf` url = serveRelURL "text/css"
        | ".pdf"    `B.isSuffixOf` url = serveRelURL "application/pdf"
        | ".eot"    `B.isSuffixOf` url = serveRelURL "application/vnd.ms-fontobject"
        | ".ttf"    `B.isSuffixOf` url = serveRelURL "application/octet-stream"
        | ".woff"   `B.isSuffixOf` url = serveRelURL "application/font-woff"
        | ".woff2"  `B.isSuffixOf` url = serveRelURL "application/font-woff2"
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
    checkRedirect = do
        exist <- fileExist (mconcat ["output", B.init url, ".html"])
        return $ if exist
            then redirectPermanent (mconcat [host, B.init url])
            else notFound

-- HTTP responses

notFound :: Wai.Response
notFound = Wai.responseLBS status404
    [(hContentType, "text/plain")]
    "Page not found"

notModified :: Wai.Response
notModified = Wai.responseLBS status304 [] ""

redirectPermanent :: ByteString -> Wai.Response
redirectPermanent path = Wai.responseLBS status301
    [ (hContentType, "text/html")
    , (hLocation, path)
    ] $ mconcat
        [ "<html><head><title>Moved</title></head><body>"
        , "Redirect to: <a href=\"" , lpath , "\">" , lpath
        , "</a></body></html>"
        ]
  where
    lpath = fromStrict path

-- utilities

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
