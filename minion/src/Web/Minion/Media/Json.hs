module Web.Minion.Media.Json where

import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import Data.Aeson qualified as Aeson
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString.Builder qualified as Bytes.Builder
import Data.List.NonEmpty qualified as Nel
import Data.Text qualified as Text
import Network.HTTP.Media qualified as Http
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Web.Minion.Media
import Web.Minion.Request.Body
import Web.Minion.Response.Body (Encode (encode))

data Json

instance ContentType Json where
  media =
    "application" Http.// "json" Http./: ("charset", "utf-8")
      Nel.:| ["application" Http.// "json"]

instance (FromJSON a) => Decode Json a where
  decode = first Text.pack . eitherDecode

instance (ToJSON a) => Encode Json a where
  encode a =
    Wai.responseBuilder
      Http.status200
      [(Http.hContentType, Http.renderHeader $ Nel.head $ media @Json)]
      (Bytes.Builder.lazyByteString $ Aeson.encode a)
