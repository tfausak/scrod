module Scrod.Extra.Http where

import qualified Control.Exception as Exception
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Tls
import qualified Network.HTTP.Types.Status as Status

fetch :: Text.Text -> IO (Either String String)
fetch url = do
  manager <- Tls.newTlsManager
  Exception.catch (fetchWithManager manager url) handleException

fetchWithManager :: Http.Manager -> Text.Text -> IO (Either String String)
fetchWithManager manager url = do
  request <- Http.parseRequest (Text.unpack url)
  response <- Http.httpLbs request manager
  let status = Status.statusCode (Http.responseStatus response)
  if status >= 200 && status < 300
    then pure . Right . decodeBody $ Http.responseBody response
    else pure . Left $ "HTTP error: status " <> show status

decodeBody :: LazyByteString.ByteString -> String
decodeBody = Text.unpack . Encoding.decodeUtf8Lenient . LazyByteString.toStrict

handleException :: Http.HttpException -> IO (Either String String)
handleException = pure . Left . Exception.displayException
