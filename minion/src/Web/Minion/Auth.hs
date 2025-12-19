{-# LANGUAGE DeriveFunctor #-}

module Web.Minion.Auth (auth, Auth (..), AuthResult (..), IsAuth (..), UnwindAuth (..)) where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Void (Void, absurd)
import Network.Wai qualified as Wai
import Web.Minion.Args (GetByType (getByType), WithReq, Args)
import Web.Minion.Error
import Web.Minion.Introspect qualified as I
import Web.Minion.Request
import Web.Minion.Router

newtype Auth (auths :: [Type]) a = Auth a

instance IsRequest (Auth auths a) where
  type RequestValue (Auth auths a) = a
  getRequestValue (Auth a) = a

data AuthResult a
  = Indefinite
  | BadAuth Text
  | Authenticated a
  deriving (Functor, Eq, Ord, Show)

class UnwindAuth (ctx :: Type) (auths :: [Type]) m a where
  unwindAuth :: [Args ctx -> ErrorBuilder -> Wai.Request -> m (AuthResult a)]

class IsAuth (auth :: Type) m a where
  type Settings auth m a :: Type
  toAuth :: Settings auth m a -> ErrorBuilder -> Wai.Request -> m (AuthResult a)

instance
  ( IsAuth auth m a
  , UnwindAuth ctx auths m a
  , GetByType (Settings auth m a) ctx
  ) =>
  UnwindAuth ctx (auth ': auths) m a
  where
  {-# INLINE unwindAuth #-}
  unwindAuth = (toAuth @auth . getByType) : (unwindAuth @ctx @auths)

instance UnwindAuth ctx '[] m a where
  {-# INLINE unwindAuth #-}
  unwindAuth = []

{- | Standard combinator for authentication. An authentication method must implement 'IsAuth'.

     See modules "Web.Minion.Auth.Basic" and "Web.Minion.Examples.BasicAuth" for examples.
-}
{-# INLINE auth #-}
auth ::
  forall auths a m ctx ts i.
  (I.Introspection i I.Request (Auth auths a)) =>
  (UnwindAuth ctx auths m a) =>
  (MonadThrow m) =>
  -- | Context with auths settings
  m (Args ctx) ->
  -- |  Handle non-Authenticated.
  (MakeError -> AuthResult Void -> m Void) ->
  ValueCombinator i (WithReq m (Auth auths a)) ts m
auth ctxm cont = Request \errorBuilder req -> do
  ctx <- ctxm
  let
    {-# INLINE go #-}
    go [] = pure Indefinite
    go (a : as) =
      a ctx errorBuilder req >>= \case
        Indefinite -> go as
        r -> pure r
  go auths
    >>= fmap Auth . \case
      Authenticated a -> pure a
      BadAuth a -> absurd <$> cont (errorBuilder req) (BadAuth a)
      Indefinite -> absurd <$> cont (errorBuilder req) Indefinite
 where
  auths = unwindAuth @ctx @auths @m @a
