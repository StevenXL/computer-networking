{-# LANGUAGE OverloadedStrings #-}

module Web.Response (Response, toByteString, response400) where

import qualified Data.ByteString.Char8 as C

data Response = Response { responseStatusCode :: Int
                         , responseBody       :: Maybe C.ByteString
                         }

response400 :: Response
response400 = Response 400 Nothing

toByteString :: Response -> C.ByteString
toByteString (Response code mBody) = C.unlines [statusLine, "Content-Type: text/html"]
  where statusLine :: C.ByteString
        statusLine = C.unwords ["HTTP/1.1", toBS code, reasonPhrase code]

reasonPhrase :: Int -> C.ByteString
reasonPhrase 400 = "Bad Request"

toBS :: Show a => a -> C.ByteString
toBS = C.pack . show
