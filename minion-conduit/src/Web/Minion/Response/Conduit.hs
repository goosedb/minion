module Web.Minion.Response.Conduit where

import Conduit qualified as C
import Control.Monad ((>=>))
import Data.ByteString.Builder qualified as Data.ByteString
import Data.Conduit.Combinators qualified as C
import Web.Minion.Codec.Encode (EncodeStream (..))

newtype ConduitResponse = ConduitResponse (C.ConduitT () Data.ByteString.Builder IO ())

instance (Applicative m) => EncodeStream m ct ConduitResponse where
  encodeStream (ConduitResponse stream) = pure
    \write flush -> C.runConduit $ stream C..| C.mapM_ (write >=> const flush) C..| C.sinkNull
