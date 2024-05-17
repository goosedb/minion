module Web.Minion.Response.Conduit where

import Conduit qualified as C
import Control.Monad ((>=>))
import Data.ByteString.Builder qualified as Data.ByteString
import Data.Conduit.Combinators qualified as C
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Web.Minion.Response

newtype ConduitResponse = ConduitResponse (C.ConduitT () Data.ByteString.Builder IO ())

instance (Monad m) => ToResponse m ConduitResponse where
  toResponse _ (ConduitResponse c) = pure $ Wai.responseStream
    Http.status200
    []
    \write flush -> C.runConduit $ c C..| C.mapM_ (write >=> const flush) C..| C.sinkNull

instance CanRespond ConduitResponse where
  canRespond _ = True
