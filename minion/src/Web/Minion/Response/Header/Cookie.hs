{-# LANGUAGE TupleSections #-}

module Web.Minion.Response.Header.Cookie where

import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Network.Wai qualified as Wai
import Web.Cookie qualified as Cookie
import Web.Minion.Auth.Cookie (IsCookie (cookieName))
import Web.Minion.Response (CanRespond (..), ToResponse (..))

data WithCookie a = WithCookie {setCookie :: [Cookie.SetCookie], response :: a}

instance (CanRespond a) => CanRespond (WithCookie a) where
  canRespond = canRespond @a

instance (ToResponse m a, Monad m) => ToResponse m (WithCookie a) where
  toResponse accept WithCookie{..} =
    Wai.mapResponseHeaders (cookieHeaders <>)
      <$> toResponse accept response
   where
    cookieHeaders = ("Set-Cookie",) . Cookie.renderSetCookieBS <$> setCookie

class (IsCookie a) => ToCookie a where
  encodeCookie :: a -> Text

initCookie :: forall a. (ToCookie a) => a -> Cookie.SetCookie
initCookie a =
  Cookie.defaultSetCookie
    { Cookie.setCookieName = Text.encodeUtf8 $ cookieName @a
    , Cookie.setCookieValue = Text.encodeUtf8 $ encodeCookie a
    }
