{-# LANGUAGE PartialTypeSignatures #-}

module Web.Minion.Response.ServerEvent (
  EventSource (..),
  ToServerEvent (..),
  Wai.ServerEvent (..),
  EventStream (..),
  showEvent,
) where

import Data.Function (fix)
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Network.Wai.EventSource qualified as Wai
import Network.Wai.EventSource.EventStream qualified as Wai
import Web.Minion

import Control.Exception (SomeException)
import Control.Monad (forM_, mfilter)
import Data.Binary.Builder qualified as Builder
import Data.Binary.Builder qualified as Bytes.Builder
import Data.ByteString qualified as Bytes
import Data.ByteString.Char8 qualified as Bytes.Char8
import Data.ByteString.Lazy qualified as Bytes.Lazy
import Data.ByteString.Lazy.Char8 qualified as Bytes.Lazy.Char8
import Data.Data (Typeable)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes, isJust)
import GHC.IO (catchException)
import Network.HTTP.Client qualified as Http
import Network.HTTP.Media
import Streaming qualified as Stream
import Streaming.ByteString qualified as Stream.Bytes
import Streaming.ByteString.Char8 qualified as Stream.Bytes
import Streaming.Prelude qualified as Stream
import Text.Read (readMaybe)
import Web.Minion.Client.Types (
  ClientError (..),
  ResponseClient (..),
  ResponseStream (..),
 )

data EventSource a = EventSource {poll :: IO (NonEmpty a), after :: Maybe SomeException -> IO ()}

data EventStream a = EventStream {getEventStream :: Stream.Stream (Stream.Of a) IO (), closeResponse :: IO ()}

instance (Typeable a, FromServerEvent a) => ResponseClient (EventSource a) where
  type ResponseForClient (EventSource a) = EventStream a
  acceptResponse resp =
    pure
      if Http.responseStatus resp == Http.status200 then Right $ parseServerEvent @a resp else Left (UnexpectedCode $ ResponseStream <$> resp)

showEvent :: Wai.ServerEvent -> String
showEvent = \case
  Wai.ServerEvent{..} -> show $ map Builder.toLazyByteString $ catMaybes (eventName : eventId : map Just eventData)
  Wai.CommentEvent{..} -> show $ Builder.toLazyByteString eventComment
  Wai.RetryEvent{..} -> show eventRetry
  Wai.CloseEvent -> ""

data EventStreamPiece
  = IdPiece Bytes.Lazy.ByteString
  | NamePiece Bytes.Lazy.ByteString
  | DataPiece Bytes.Lazy.ByteString
  | CommentPiece Bytes.Lazy.ByteString
  | RetryPiece Int
  | EmptyPiece
  deriving (Eq)

parseServerEvent :: forall a. (FromServerEvent a) => Http.Response (IO Bytes.Char8.ByteString) -> EventStream a
parseServerEvent response =
  EventStream parsedEventStream (Http.responseClose response)
 where
  reader = Http.responseBody response
  parseLine line
    | Bytes.Lazy.null line = Just EmptyPiece
    | Just v <- Bytes.Lazy.stripPrefix "data:" line = Just $ DataPiece v
    | Just v <- Bytes.Lazy.stripPrefix "event:" line = Just $ NamePiece v
    | Just v <- Bytes.Lazy.stripPrefix "id:" line = Just $ IdPiece v
    | Just v <- Bytes.Lazy.stripPrefix ":" line = Just $ CommentPiece v
    | Just v <- Bytes.Lazy.stripPrefix "retry:" line
    , s <- Bytes.Lazy.Char8.unpack v
    , Just n <- readMaybe s =
        Just $ RetryPiece n
    | otherwise = Nothing
  linesStream =
    Stream.mapped Stream.Bytes.toLazy
      . Stream.Bytes.lines
      . Stream.Bytes.reread (const $ mfilter (not . Bytes.null) . Just <$> reader)
      $ ()
  piecesStream = Stream.mapMaybe parseLine linesStream
  piecesBatchStream = Stream.split EmptyPiece piecesStream
  eventStream = Stream.mapped (Stream.fold parseEvent Nothing id) piecesBatchStream
  parsedEventStream = Stream.mapM (either throwM pure . fromServerEvent @a) $ Stream.catMaybes eventStream
  parseEvent Nothing = \case
    IdPiece i -> Just $ Wai.ServerEvent Nothing (Just $ bs i) []
    NamePiece n -> Just $ Wai.ServerEvent (Just $ bs n) Nothing []
    DataPiece d -> Just $ Wai.ServerEvent Nothing Nothing [bs d]
    CommentPiece c -> Just $ Wai.CommentEvent (bs c)
    RetryPiece n -> Just $ Wai.RetryEvent n
    EmptyPiece -> Nothing
  parseEvent (Just Wai.ServerEvent{..}) = \case
    IdPiece i -> Just Wai.ServerEvent{eventId = Just $ bs i, ..}
    NamePiece n -> Just Wai.ServerEvent{eventName = Just $ bs n, ..}
    DataPiece d -> Just Wai.ServerEvent{eventData = eventData <> [bs d], ..}
    _ -> Nothing
  parseEvent (Just a) = const (Just a)

  bs = Bytes.Builder.fromLazyByteString

class ToServerEvent a where
  toServerEvent :: a -> Wai.ServerEvent

class FromServerEvent a where
  fromServerEvent :: Wai.ServerEvent -> Either SomeException a

instance FromServerEvent Wai.ServerEvent where
  fromServerEvent = Right

instance ToServerEvent Wai.ServerEvent where
  toServerEvent = id

textEventStream :: MediaType
textEventStream = "text" // "event-stream"

instance CanRespond (EventSource a) where
  canRespond [] = True
  canRespond l = any (isJust . matchAccept [textEventStream]) l

instance (Monad m, ToServerEvent a) => ToResponse m (EventSource a) where
  toResponse _ (EventSource poll after) = do
    pure $ Wai.responseStream
      Http.status200
      [(Http.hContentType, renderHeader textEventStream)]
      \write flush -> catchException
        do
          flush >> fix \continue -> do
            events <- poll
            forM_ (Wai.eventToBuilder . toServerEvent <$> events) \case
              Nothing -> after Nothing
              Just e -> write e
            flush >> continue
        do after . Just
