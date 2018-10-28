{-# LANGUAGE OverloadedStrings #-}

module Web.Request (parseRawRequest, Request) where

import           Data.Text            (Text, unpack)
import           Data.Void            (Void)
import           Text.Megaparsec      (Parsec, many, parseMaybe)
import           Text.Megaparsec.Char (notChar, spaceChar, string, anyChar)

data Method = GET deriving (Show, Read)

data Request = Request { method :: Method, path :: FilePath } deriving Show

type Parser = Parsec Void Text

parseRawRequest :: Text -> Maybe Request
parseRawRequest = const Nothing

requestParser :: Parser Request
requestParser = do
  method <- methodParser
  _      <- spaceChar
  path   <- pathParser
  _      <- many anyChar -- need to consume rest of Text; otherwise fail
  return $ Request method path

methodParser :: Parser Method
methodParser = (read . unpack) <$> string "GET"

pathParser :: Parser FilePath
pathParser = many $ notChar ' '
