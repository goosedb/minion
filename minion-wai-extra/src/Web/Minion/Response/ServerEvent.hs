module Web.Minion.Response.ServerEvent (
  EventSource (..),
  ToServerEvent (..),
  Wai.ServerEvent (..),
) where

import Data.Function (fix)
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Network.Wai.EventSource qualified as Wai
import Network.Wai.EventSource.EventStream qualified as Wai
import Web.Minion

import Data.Maybe (isJust)
import Network.HTTP.Media

newtype EventSource a = EventSource (IO a)

class ToServerEvent a where
  toServerEvent :: a -> Wai.ServerEvent

instance ToServerEvent Wai.ServerEvent where
  toServerEvent = id

textEventStream :: MediaType
textEventStream = "text" // "event-stream"

instance CanRespond (EventSource a) where
  canRespond [] = True
  canRespond l = any (isJust . matchAccept [textEventStream]) l

instance (Monad m, ToServerEvent a) => ToResponse m (EventSource a) where
  toResponse _ (EventSource poll) = do
    pure $ Wai.responseStream
      Http.status200
      [(Http.hContentType, renderHeader textEventStream)]
      \write flush ->
        flush >> fix \continue -> do
          event <- poll
          case Wai.eventToBuilder $ toServerEvent event of
            Nothing -> pure ()
            Just e -> write e >> flush >> continue
