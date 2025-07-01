module Web.Minion.Request.Conduit (ConduitRequest (..)) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString qualified as Bytes
import Data.Conduit qualified as C
import Data.Conduit.Combinators qualified as C
import Web.Minion.Codec.Decode (DecodeStream(..))

newtype ConduitRequest m = ConduitRequest (C.ConduitT () Bytes.ByteString m ())

instance MonadIO m => DecodeStream ct (ConduitRequest m) where
  decodeStream chunks = pure $ Right $ ConduitRequest $ C.repeatWhileM (liftIO @m chunks) (not . Bytes.null)

