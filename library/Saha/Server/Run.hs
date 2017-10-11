module Saha.Server.Run
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
import System.Posix.ByteString (RawFilePath, fileExist)

import Network.HTTP.Types
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import Saha.Server.UnixSocket
import Saha.Server.ModifiedTime

(++) :: Monoid m => m -> m -> m
(++) = mappend

-- OpenAt configuration

data OpenAt
    = OpenAtPort Warp.Port
    | OpenAtUnixSocket RawFilePath

parseOpenAt :: String -> OpenAt
parseOpenAt s = case readMaybe s :: Maybe Warp.Port of
    Just p  -> OpenAtPort p
    Nothing -> OpenAtUnixSocket (B.pack s)

instance Show OpenAt where
    show (OpenAtPort port) = "port " ++ show port
    show (OpenAtUnixSocket path) = "Unix socket " ++ show path

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
app absoluteHost req respond = if isSafeURL url
    then serveNormal absoluteHost req >>= respond
    else respond notFound
  where
    url = Wai.rawPathInfo req

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
    serveStatic = serve (mimetype (extension url)) (B.drop 1 url)
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
    checkRedirect = fileExist htmlpath >>= \e -> return $ if e
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
    , c `B.elem` "-_./~"
    ]
  where
    n = ord c

mimetype :: ByteString -> ByteString
mimetype ext = case ext of
    "jpg"   -> "image/jpeg"
    "png"   -> "image/png"
    "svg"   -> "image/svg+xml;charset=utf-8"
    "js"    -> "application/javascript;charset=utf-8"
    "css"   -> "text/css;charset=utf-8"
    "pdf"   -> "application/pdf"
    "eot"   -> "application/vnd.ms-fontobject"
    "ttf"   -> "application/octet-stream"
    "woff"  -> "application/font-woff"
    "woff2" -> "application/font-woff2"
    _       -> "application/octet-stream"

cExtensionLongest :: Int
cExtensionLongest = 5

extension :: ByteString -> ByteString
extension path = snd $ B.breakEnd (== '.') $
    B.drop (B.length path - cExtensionLongest) path

