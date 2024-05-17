module Web.Minion.Raw where

import Data.ByteString qualified as Bytes
import Data.ByteString.Lazy qualified as Bytes.Lazy
import Web.Minion.Request

newtype LazyBytes = LazyBytes Bytes.Lazy.ByteString

newtype Chunks = Chunks (IO Bytes.ByteString)

instance IsRequest LazyBytes where
  type RequestValue LazyBytes = Bytes.Lazy.ByteString
  getRequestValue (LazyBytes a) = a

instance IsRequest Chunks where
  type RequestValue Chunks = IO Bytes.ByteString
  getRequestValue (Chunks a) = a
