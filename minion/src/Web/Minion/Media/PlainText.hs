module Web.Minion.Media.PlainText where

import Data.List.NonEmpty qualified as Nel
import Network.HTTP.Media qualified as Http
import Web.Minion.Media

data PlainText

instance ContentType PlainText where
  media = "text" Http.// "plain" Http./: ("charset", "utf-8") Nel.:| []
