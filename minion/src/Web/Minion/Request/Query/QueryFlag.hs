module Web.Minion.Request.Query.QueryFlag (QueryFlag (..), QueryFlagStrict (..)) where

import Control.Applicative (asum)
import Control.Monad.Catch (MonadThrow)
import Web.Minion.Args.Internal
import Web.Minion.Introspect qualified as I
import Web.Minion.Request.Query.Internal
import Web.Minion.Router.Internal

newtype QueryFlag = QueryFlag Bool

class QueryFlagStrict presence where
  queryFlag ::
    forall m i ts.
    (I.Introspection i I.QueryParam QueryFlag, MonadThrow m) =>
    -- | .
    QueryParamName ->
    ValueCombinator i (WithQueryParam presence Strict m QueryFlag) ts m
  mapFlag :: forall a. (QueryFlag -> a) -> Arg presence Strict QueryFlag -> Arg presence Strict a

instance QueryFlagStrict Required where
  queryFlag qn = withQueryParam qn \makeError ->
    maybe
      (badReq makeError noKeyError qn)
      (maybe (pure (QueryFlag True)) (pure . QueryFlag . parseQueryFlag) . asum)
  mapFlag = ($)

instance QueryFlagStrict Optional where
  queryFlag qn = withQueryParam qn \_ ->
    maybe
      (pure Nothing)
      (fmap Just . maybe (pure (QueryFlag True)) (pure . QueryFlag . parseQueryFlag) . asum)
  mapFlag = fmap
