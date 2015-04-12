module ModifiedTime
    ( getMTimeForHTTP
    ) where

import System.Directory (getModificationTime)
import Data.Time (defaultTimeLocale, formatTime)

getMTimeForHTTP :: FilePath -> IO String
getMTimeForHTTP path = fmap
    (formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT")
    (getModificationTime path)
