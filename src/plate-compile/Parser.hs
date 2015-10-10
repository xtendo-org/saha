{-# LANGUAGE OverloadedStrings #-}

module Parser
    ( source
    , template
    , Template(..)
    ) where

import Control.Applicative

import Data.Char
import Data.Text
import Data.Attoparsec.Text

source :: Text -> Maybe ([(Text, Text)], Text)
source x = case parse headers x of
    Done rest r -> Just (r, rest)
    _ -> Nothing

headers :: Parser [(Text, Text)]
headers = manyTill header (char '\n')

header :: Parser (Text, Text)
header = do
    key <- takeTill (':' ==)
    _ <- anyChar
    _ <- takeTill $ not . isSpace
    value <- takeTill ('\n' ==)
    _ <- anyChar
    return (key, value)

data Template
    = TextSegment Text
    | Variable Text
    deriving Show

template :: Text -> [Template]
template x = case parseOnly segments x of
    Right ts -> ts
    Left err -> error $ "template parse fail: " ++ err

segments :: Parser [Template]
segments = manyTill (varSeg <|> textSeg) endOfInput
  where
    varSeg = do
        _ <- char '\\'
        var <- takeTill ('\\' ==)
        _ <- anyChar
        return $ Variable var
    textSeg = fmap TextSegment $ takeTill ('\\' ==)
