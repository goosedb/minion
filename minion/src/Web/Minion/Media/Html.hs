module Web.Minion.Media.Html where

import Data.List.NonEmpty qualified as Nel
import Network.HTTP.Media qualified as Http
import Web.Minion.Media (ContentType (..))

data Html

instance ContentType Html where
  media = "text" Http.// "html" Nel.:| []
