{-# LANGUAGE InstanceSigs #-}

module Web.Minion.Request.Header where

import Control.Monad.Catch (MonadThrow (..))
import Data.ByteString qualified as Bytes
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.String.Conversions (ConvertibleStrings (..))
import Data.Text (Text)
import Network.HTTP.Types qualified as Http
import Web.Minion.Args.Internal
import Web.Minion.Introspect qualified as I
import Web.Minion.Request.Header.Internal
import Web.Minion.Router.Internal

class HeaderStrict presence where
  header ::
    forall a m i ts.
    (I.Introspection i I.Header a, MonadThrow m) =>
    -- | .
    Http.HeaderName ->
    (MakeError -> NonEmpty Bytes.ByteString -> m a) ->
    ValueCombinator i (WithHeader presence Strict m a) ts m

instance HeaderStrict Required where
  header hn f = Header hn \makeError ->
    maybe
      (throwM . makeError Http.status400 . convertString $ headerNotFoundError hn)
      (f makeError)
      . nonEmpty

instance HeaderStrict Optional where
  header hn handle = Header hn (\mk -> maybe (pure Nothing) (fmap Just . handle mk) . nonEmpty)

class HeaderLenient presence where
  headerLenient ::
    (I.Introspection i I.Header a, MonadThrow m) =>
    -- | .
    Http.HeaderName ->
    (MakeError -> NonEmpty Bytes.ByteString -> m (Either Text a)) ->
    ValueCombinator i (WithHeader presence (Lenient Text) m a) ts m

instance HeaderLenient Required where
  headerLenient ::
    (I.Introspection i I.Header a, MonadThrow m) =>
    -- \| .
    Http.HeaderName ->
    (MakeError -> NonEmpty Bytes.ByteString -> m (Either Text a)) ->
    ValueCombinator i (WithHeader Required (Lenient Text) m a) ts m
  headerLenient hn f = Header hn \makeError ->
    maybe
      (throwM . makeError Http.status400 . convertString $ headerNotFoundError hn)
      (f makeError)
      . nonEmpty

instance HeaderLenient Optional where
  headerLenient ::
    (I.Introspection i I.Header a, MonadThrow m) =>
    -- \| .
    Http.HeaderName ->
    (MakeError -> NonEmpty Bytes.ByteString -> m (Either Text a)) ->
    ValueCombinator i (WithHeader Optional (Lenient Text) m a) ts m
  headerLenient hn f = Header hn (\mkErr -> maybe (pure Nothing) (fmap Just . f mkErr) . nonEmpty)
