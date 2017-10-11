module Saha.Server.UnixSocket
    ( Socket
    , unixSocket
    ) where

import qualified Data.ByteString.Char8 as B
import Control.Monad
import System.IO.Error
import Network.Socket
import System.Posix.ByteString

unixSocket :: RawFilePath -> IO Socket
unixSocket path = do
    tryRemoveFile path
    s <- socket AF_UNIX Stream defaultProtocol
    bind s (SockAddrUnix (B.unpack path))
    setFileMode path accessModes
    listen s maxListenQueue
    return s

tryRemoveFile :: RawFilePath -> IO ()
tryRemoveFile path = catchIOError (removeLink path) $
    \ e -> unless (isDoesNotExistError e) $ ioError e

