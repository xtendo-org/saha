module Saha.Constants where

import Data.ByteString
import Data.Text
import Data.Text.Encoding


redirectMagicText :: Text
redirectMagicText = "SAHA REDIRECT "

redirectMagicBytes :: ByteString
redirectMagicBytes = encodeUtf8 redirectMagicText

chunkSize :: Int
chunkSize = 4096
