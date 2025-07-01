module Web.Minion.Media.Json where

import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import Data.Aeson qualified as Aeson
import Data.Bifunctor (Bifunctor (..))
import Data.List.NonEmpty qualified as Nel
import Data.Text qualified as Text
import Network.HTTP.Media qualified as Http
import Web.Minion.Media
import Web.Minion.Request.Body

data Json

instance ContentType Json where
  media =
    "application" Http.// "json" Http./: ("charset", "utf-8")
      Nel.:| ["application" Http.// "json"]

instance (FromJSON a) => Decode Json a where
  decode = first Text.pack . eitherDecode

instance (ToJSON a) => Encode Json a where
  encode = Aeson.encode
