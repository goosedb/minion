{-# LANGUAGE TupleSections #-}

module Web.Minion.Response.Header.Cookie where

import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Web.Cookie qualified as Cookie
import Web.Minion.Response (CanRespond (..), ToResponse (..))

data WithCookie a = WithCookie {setCookie :: [Cookie.SetCookie], response :: a}

instance (CanRespond a) => CanRespond (WithCookie a) where
  canRespond = canRespond @a

instance (ToResponse m a, Monad m) => ToResponse m (WithCookie a) where
  toResponse accept WithCookie{..} =
    Wai.mapResponseHeaders (cookieHeaders <>)
      <$> toResponse accept response
   where
    cookieHeaders = (Http.hCookie,) . Cookie.renderSetCookieBS <$> setCookie
