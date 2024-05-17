module Web.Minion.Request.Header where

import Control.Monad.Catch (MonadThrow (..))
import Data.ByteString qualified as Bytes
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.String.Conversions (ConvertibleStrings (..))
import Network.HTTP.Types qualified as Http
import Web.Minion.Args.Internal
import Web.Minion.Introspect qualified as I
import Web.Minion.Request.Header.Internal
import Web.Minion.Router.Internal

{-# INLINE header' #-}
header' ::
  (I.Introspection i I.Header a, MonadThrow m) =>
  -- | .
  Http.HeaderName ->
  (MakeError -> NonEmpty Bytes.ByteString -> m a) ->
  ValueCombinator i (WithHeader Required Strict m a) ts m
header' hn f =
  Header
    hn
    \makeError ->
      maybe
        (throwM . makeError Http.status400 . convertString $ headerNotFoundError hn)
        (f makeError)
        . nonEmpty

{-# INLINE headerLenient' #-}
headerLenient' ::
  (I.Introspection i I.Header a, MonadThrow m) =>
  -- | .
  Http.HeaderName ->
  (MakeError -> NonEmpty Bytes.ByteString -> m (Either e a)) ->
  ValueCombinator i (WithHeader Required (Lenient e) m a) ts m
headerLenient' hn f =
  Header
    hn
    \makeError ->
      maybe
        (throwM . makeError Http.status400 . convertString $ headerNotFoundError hn)
        (f makeError)
        . nonEmpty

{-# INLINE headerLenient #-}
headerLenient ::
  (I.Introspection i I.Header a) =>
  -- | .
  Http.HeaderName ->
  (MakeError -> [Bytes.ByteString] -> m (Maybe (Either e a))) ->
  ValueCombinator i (WithHeader Optional (Lenient e) m a) ts m
headerLenient = Header

{-# INLINE header #-}
header ::
  (I.Introspection i I.Header a) =>
  -- | .
  Http.HeaderName ->
  (MakeError -> [Bytes.ByteString] -> m (Maybe a)) ->
  ValueCombinator i (WithHeader Optional Strict m a) ts m
header = Header
