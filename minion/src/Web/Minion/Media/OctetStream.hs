module Web.Minion.Media.OctetStream where

import Data.List.NonEmpty qualified as Nel
import Network.HTTP.Media qualified as Http
import Web.Minion.Media (ContentType (media))

data OctetStream a

instance ContentType (OctetStream a) where
  media = "application" Http.// "octet-stream" Nel.:| []

data Bytes
data Chunks
