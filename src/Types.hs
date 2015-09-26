module Types where

import Text.Read (readMaybe)

import qualified Network.Wai.Handler.Warp as Warp

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
