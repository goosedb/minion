module Web.Minion.Response (CanRespond (..), ToResponse (..), NoBody (..), IsResponse) where

import Data.ByteString qualified as Bytes
import Data.ByteString.Builder qualified as Bytes.Builder
import Data.Function (fix)
import Data.Maybe (isJust)
import Network.HTTP.Media
import Network.HTTP.Types
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Http
import Network.Wai qualified as Wai
import Web.Minion.Raw

type IsResponse m o = (CanRespond o, ToResponse m o)

class CanRespond o where
  canRespond ::
    -- | Accept header values
    [Bytes.ByteString] ->
    Bool

class ToResponse m r where
  toResponse :: [Bytes.ByteString] -> r -> m Http.Response

data NoBody = NoBody

instance CanRespond NoBody where
  canRespond _ = True

instance (Monad m) => ToResponse m NoBody where
  toResponse _ _ = pure (Http.responseBuilder Http.status200 [] mempty)

applicationOctetStream :: MediaType
applicationOctetStream = "application" // "octet-stream"

instance CanRespond Chunks where
  canRespond [] = True
  canRespond l = any (isJust . matchAccept [applicationOctetStream]) l

instance CanRespond LazyBytes where
  canRespond [] = True
  canRespond l = any (isJust . matchAccept [applicationOctetStream]) l

instance (Applicative m) => ToResponse m Chunks where
  toResponse _ (Chunks chunks) = pure $ Wai.responseStream
    status200
    []
    \write flush -> do
      flush
      fix \continue -> do
        ch <- chunks
        if Bytes.null ch
          then pure ()
          else write (Bytes.Builder.byteString ch) >> flush >> continue

instance (Applicative m) => ToResponse m LazyBytes where
  toResponse _ (LazyBytes bytes) =
    pure $
      Wai.responseBuilder
        status200
        []
        (Bytes.Builder.lazyByteString bytes)
