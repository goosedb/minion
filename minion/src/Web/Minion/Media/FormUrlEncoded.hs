module Web.Minion.Media.FormUrlEncoded where

import Data.List.NonEmpty qualified as Nel
import Network.HTTP.Media qualified as Http
import Web.Minion.Media

data FormUrlEncoded

instance ContentType FormUrlEncoded where
  media = "application" Http.// "x-www-form-urlencoded" Nel.:| []
