{-# LANGUAGE DeriveFunctor #-}

module Web.Minion.Auth where

import Data.Kind (Type)
import Data.Void (Void, absurd)
import Network.Wai qualified as Wai
import Web.Minion.Args (GetByType (getByType), HList, WithReq)
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
  | BadAuth
  | Authenticated a
  deriving (Functor)

class UnwindAuth (ctx :: [Type]) (auths :: [Type]) m a where
  unwindAuth :: [HList ctx -> ErrorBuilder -> Wai.Request -> m (AuthResult a)]

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

{-# INLINE auth #-}
auth ::
  forall auths a m ctx ts i.
  (I.Introspection i I.Request (Auth auths a)) =>
  (UnwindAuth ctx auths m a) =>
  (MonadThrow m) =>
  -- | Context with auths settings
  m (HList ctx) ->
  -- |  Handle non-Authenticated.
  (MakeError -> AuthResult Void -> m Void) ->
  ValueCombinator i (WithReq m (Auth auths a)) ts m
auth ctxm cont = Request \errorBuilder req -> do
  ctx <- ctxm
  let auths = unwindAuth @ctx @auths @m @a
      {-# INLINE go #-}
      go [] = pure Indefinite
      go (a : as) =
        a ctx errorBuilder req >>= \case
          Indefinite -> go as
          r -> pure r
  go auths
    >>= fmap Auth . \case
      Authenticated a -> pure a
      BadAuth -> absurd <$> cont (errorBuilder req) BadAuth
      Indefinite -> absurd <$> cont (errorBuilder req) BadAuth
