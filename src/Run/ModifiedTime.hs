module Run.ModifiedTime
    ( Modified(..)
    , EpochTime
    , getMTime
    , formattedMTime
    , modifiedSince
    ) where

import Data.ByteString (ByteString)
import System.Posix.Types (EpochTime)
import System.Posix.Files.ByteString
import Network.HTTP.Types
import Network.HTTP.Date
import Network.Wai (Request, requestHeaders)

data Modified = NotModified | Modified

getMTime :: ByteString -> IO EpochTime
getMTime = fmap modificationTime . getFileStatus

formattedMTime :: EpochTime -> ByteString
formattedMTime = formatHTTPDate . epochTimeToHTTPDate

modifiedSince :: Request -> EpochTime -> Modified
modifiedSince req mtime = case since of
    Nothing -> Modified
    Just rawReqTime -> case parseHTTPDate rawReqTime of
        Nothing -> Modified
        Just reqTime -> if reqTime < epochTimeToHTTPDate mtime
            then Modified
            else NotModified
  where
    since = lookup hIfModifiedSince (requestHeaders req)
