module Web.Minion.Examples.Conduit.UpperCase (app) where

import Data.Binary.Put qualified as Binary
import Data.Conduit ((.|))
import Data.Conduit.Combinators qualified as Conduit
import Data.Functor (($>))
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Web.Minion
import Web.Minion.Conduit

app :: ApplicationM IO
app = serve api

api :: Router Void IO
api = "api" /> "conduit" /> streamBodyBytes .> handle POST upperCase

upperCase :: ConduitRequest IO -> IO ConduitResponse
upperCase (ConduitRequest source) = pure $ ConduitResponse do
  source
    .| Conduit.decodeUtf8
    .| Conduit.map Text.toUpper
    .| Conduit.mapM (\a -> Text.IO.putStrLn a $> a)
    .| Conduit.encodeUtf8
    .| Conduit.map (Binary.execPut . Binary.putByteString)
