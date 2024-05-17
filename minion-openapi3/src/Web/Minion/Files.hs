module Web.Minion.Files where

import Data.ByteString qualified as Bytes
import Data.Text (Text)
import Web.Minion.Embed

indexTemplate :: Text
indexTemplate = $(embedIndex)

ui :: [(FilePath, Bytes.ByteString)]
ui = $(embedUi)
