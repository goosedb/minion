{-# LANGUAGE PartialTypeSignatures #-}

module Web.Minion.Websocket.Internal where

import Control.Monad ((>=>))
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Network.Wai.Handler.WebSockets qualified as Websocket
import Network.WebSockets qualified as Websocket
import Web.Minion
import Web.Minion.Introspect qualified as I
import Web.Minion.Request (IsRequest (..))
import Web.Minion.Router (Router' (..))

newtype WebsocketRequest = WebsocketRequest
  { requestHead :: Websocket.RequestHead
  }

newtype WebsocketHandler = WebsocketHandler Wai.Response

instance IsRequest WebsocketRequest where
  type RequestValue WebsocketRequest = WebsocketRequest
  getRequestValue = id

instance (Applicative m) => ToResponse m WebsocketHandler where
  toResponse _ (WebsocketHandler a) = pure a

instance CanRespond WebsocketHandler where
  canRespond = const True

websocket ::
  forall ts st m i.
  ( HandleArgs (ts :+ WithReq m WebsocketRequest) st m
  , I.Introspection i 'I.Response WebsocketHandler
  , Monad m
  , I.Introspection i I.Request WebsocketRequest
  , MonadThrow m
  ) =>
  (DelayedArgs st ~> m WebsocketHandler) -> Router' i ts m
websocket f = websocketReq .> handle @WebsocketHandler GET f
 where
  websocketReq ::
    (I.Introspection i I.Request WebsocketRequest, Monad m, MonadThrow m) =>
    Router' i (ts :+ WithReq m WebsocketRequest) m -> Router' i ts m
  websocketReq = Request \_ req ->
    if Websocket.isWebSocketsReq req
      then pure $ WebsocketRequest (Websocket.getRequestHead req)
      else throwM (NoMatch Nothing)

withPendingConnection :: (Monad m) => Websocket.ConnectionOptions -> WebsocketRequest -> (Websocket.PendingConnection -> IO ()) -> m WebsocketHandler
withPendingConnection opts WebsocketRequest{..} =
  pure
    . WebsocketHandler
    . flip Wai.responseRaw (Wai.responseLBS Http.status500 [] "")
    . Websocket.runWebSockets opts requestHead

withConnection :: (Monad m) => Websocket.ConnectionOptions -> WebsocketRequest -> (Websocket.Connection -> IO ()) -> m WebsocketHandler
withConnection opts req action = withPendingConnection opts req (Websocket.acceptRequest >=> action)
