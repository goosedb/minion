module Web.Minion.Request.Query.Flag (QueryFlag (..)) where

import Control.Applicative (asum)
import Control.Monad.Catch (MonadThrow)
import Web.Minion.Args.Internal
import Web.Minion.Introspect qualified as I
import Web.Minion.Request.Query.Internal
import Web.Minion.Router.Internal

class QueryFlag presence where
  queryFlag ::
    forall m i ts.
    (I.Introspection i I.QueryParam Bool, MonadThrow m) =>
    -- | .
    QueryParamName ->
    ValueCombinator i (WithQueryParam presence Strict m Bool) ts m

instance QueryFlag Required where
  queryFlag qn = withQueryParam qn \makeError ->
    maybe
      (badReq makeError noKeyError qn)
      (maybe (pure True) (pure . parseQueryFlag) . asum)

instance QueryFlag Optional where
  queryFlag qn = withQueryParam qn \_ ->
    maybe
      (pure Nothing)
      (fmap Just . maybe (pure True) (pure . parseQueryFlag) . asum)
