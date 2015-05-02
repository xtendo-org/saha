{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad

import Data.Char
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import Network.HTTP.Types
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import ModifiedTime

c_DEBUG :: Bool
c_DEBUG = False

main :: IO ()
main = if c_DEBUG
    then Warp.runSettings
        (Warp.setFdCacheDuration 0 $ Warp.defaultSettings) app
    else Warp.runSettings Warp.defaultSettings app

app :: Wai.Application
app req respond = do
    when c_DEBUG $ print url
    if isSafeURL url
        then serveNormal respond url
        else respond notFound
  where
    url = Wai.rawPathInfo req

notFound :: Wai.Response
notFound = Wai.responseLBS status404
    [(hContentType, "text/plain")]
    "Page not found"

serveNormal
    :: (Wai.Response -> IO Wai.ResponseReceived)
    -> ByteString -> IO Wai.ResponseReceived
serveNormal respond url
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
    serve ctype path = do
        stamp <- fmap B.pack $ getMTimeForHTTP (B.unpack path)
        respond $ Wai.responseFile status200
            [ (hContentType, ctype)
            , (hLastModified, stamp)
            ]
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
