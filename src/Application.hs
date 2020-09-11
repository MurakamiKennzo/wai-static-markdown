{-# LANGUAGE OverloadedStrings #-}

module Application
  (
    application
  ) where

import Data.Text
import qualified Data.Text.Lazy as DTL
import Data.Text.Lazy.Encoding
import Network.Wai
import Network.HTTP.Types
import CMark
import Control.Applicative
import Control.Exception
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as BL

application :: Application
application request respond
  | method == methodGet = staticServer path >>= respond
  | otherwise = respond badRequest
  where path = pathInfo request
        method = requestMethod request

staticServer :: [Text] -> IO Response
staticServer path = (flip catch) (return $ pure notFound :: IOException -> IO Response) $ do
  let path' = intercalate "/" path
      pathA = unpack $ serverDir <> path' <> "/index.md"
      pahtB = unpack $ serverDir <> path' <> ".md"
  content <- BL.readFile pathA <|> BL.readFile pahtB
  return $ responseLBS
            status200
            [("Content-Type", "text/html")] $
            encodeUtf8 . DTL.fromStrict . commonmarkToHtml [] . DTL.toStrict $ decodeUtf8 content

serverDir :: Text
serverDir = "static/"

notFound :: Response
notFound = responseBuilder
            notFound404
            [] $
            byteString $ statusMessage notFound404

badRequest :: Response
badRequest = responseBuilder
              badRequest400
              [] $ 
              byteString $ statusMessage badRequest400
