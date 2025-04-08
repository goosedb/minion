module Web.Minion.Request.Query.QueryFlag.Static where

import Control.Monad.Catch (MonadThrow)
import Data.Coerce (coerce)
import Data.Data (Proxy (..))
import Data.Text qualified as Text
import GHC.TypeLits (KnownSymbol, symbolVal)
import Web.Minion.Args.Internal
import Web.Minion.Introspect qualified as I
import Web.Minion.Request.Query.QueryFlag qualified as Q
import Web.Minion.Router.Internal

newtype QueryFlag name = QueryFlag Bool

queryFlag ::
  forall presence name m i ts.
  (I.Introspection i I.QueryParam Q.QueryFlag, MonadThrow m, Q.QueryFlagStrict presence, KnownSymbol name) =>
  -- | .
  ValueCombinator i (WithQueryParam presence Strict m (QueryFlag name)) ts m
queryFlag c =
  Q.queryFlag @presence (Text.pack $ symbolVal $ Proxy @name) do
    MapArgs
      do
        \case (WithQueryParam a) :#! as -> WithQueryParam (fmap (Q.mapFlag @presence @(QueryFlag name) coerce) a) :#! as
      do c
