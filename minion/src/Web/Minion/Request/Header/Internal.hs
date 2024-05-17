module Web.Minion.Request.Header.Internal where

import Data.ByteString
import Data.CaseInsensitive qualified as CI
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encode

headerNotFoundError :: CI.CI ByteString -> Text
headerNotFoundError hn = "Header not found: " <> headerToText hn

headerToText :: CI.CI ByteString -> Text
headerToText (Text.Encode.decodeUtf8 . CI.original -> txtHeader) = txtHeader
