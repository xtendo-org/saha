{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import Network.HTTP.Types
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = do
    Warp.runSettings (Warp.defaultSettings) app

app :: Wai.Application
app req respond = do
    print url
    respond $ if isSafeURL url
        then serveNormal url
        else notFound
  where
    url = Wai.rawPathInfo req

notFound :: Wai.Response
notFound = Wai.responseLBS status404
    [(hContentType, "text/plain")]
    "Page not found"

serveNormal :: ByteString -> Wai.Response
serveNormal url
    | url == "/static/css/main.css" = serve "text/css" $ B.drop 1 url
    | "/static/img/" `B.isPrefixOf` url = case rtake 4 url of
        ".jpg"  -> serve "image/jpeg" $ B.drop 1 url
        ".png"  -> serve "image/png" $ B.drop 1 url
        _       -> notFound
    | url == "/favicon.ico" = serve "image/vnd.microsoft.icon"
        "static/img/favicon.ico"
    | url == "/" = serve "text/html; charset=utf-8" $ htmlpath "index/"
    | "/" `B.isSuffixOf` url = serve "text/html; charset=utf-8" $ htmlpath url
    | otherwise = notFound
  where
    htmlpath path = "output/" `mappend` B.init path `mappend` ".html"
    serve ctype path = Wai.responseFile status200 [(hContentType, ctype)]
        (B.unpack path) Nothing

rtake :: Int -> ByteString -> ByteString
rtake n b = B.drop (B.length b - n) b

isIn :: ByteString -> ByteString -> Bool
isIn needle haystack = (0 /=) $ B.length $ snd $
    B.breakSubstring needle haystack

isSafeURL :: ByteString -> Bool
isSafeURL url = and
    [ B.all urlChar url
    , not $ "//" `isIn` url
    , not $ "../" `isIn` url
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
