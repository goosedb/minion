module Web.Minion.Request.Query.Internal where

import Data.Bifunctor (Bifunctor (..))
import Data.ByteString

import Data.Text (Text)

import Data.ByteString.Lazy qualified as Bytes.Lazy
import Data.String.Conversions (ConvertibleStrings (..))
import Data.Text.Encoding qualified as Text.Encoding
import Network.HTTP.Types qualified as Http
import Web.HttpApiData (FromHttpApiData (parseQueryParam))
import Web.Minion.Error (ServerError (..))
import Web.Minion.Router.Internal (MakeError)

queryParamKeyNotFoundError :: Text -> Bytes.Lazy.ByteString
queryParamKeyNotFoundError qn = convertString $ "Query param not found: " <> qn

queryParamValueNotFoundError :: Text -> Bytes.Lazy.ByteString
queryParamValueNotFoundError qn = convertString $ "Query param value not found: " <> qn

decodeQueryParam :: (FromHttpApiData a) => ByteString -> Either Text a
decodeQueryParam = parseQueryParam . Text.Encoding.decodeUtf8

decodeQueryParamOrServerError :: (FromHttpApiData a) => MakeError -> ByteString -> Either ServerError a
decodeQueryParamOrServerError makeError = first (makeError Http.status400 . convertString) . decodeQueryParam
