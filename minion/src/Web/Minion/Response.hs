module Web.Minion.Response (CanRespond (..), ToResponse (..), NoBody (..), Redirect (..), IsResponse) where

import Data.ByteString qualified as Bytes
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Web.Minion.Response.Status (IsStatus (..))

type IsResponse m o = (CanRespond o, ToResponse m o)

class CanRespond o where
  canRespond ::
    -- | Accept header values
    [Bytes.ByteString] ->
    Bool

class ToResponse m r where
  toResponse :: [Bytes.ByteString] -> r -> m Wai.Response

data NoBody status = NoBody

instance CanRespond (NoBody status) where
  canRespond _ = True

instance (Monad m, IsStatus status) => ToResponse m (NoBody status) where
  toResponse _ _ = pure (Wai.responseBuilder (httpStatus @status) [] mempty)

newtype Redirect = Redirect {location :: Text}

instance CanRespond Redirect where
  canRespond _ = True

instance (Monad m) => ToResponse m Redirect where
  toResponse _ Redirect{..} = pure $ Wai.responseLBS Http.status302 [("Location", Text.encodeUtf8 location)] mempty
