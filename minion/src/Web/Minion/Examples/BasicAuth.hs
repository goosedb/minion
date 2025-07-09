module Web.Minion.Examples.BasicAuth (app) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Data.Functor (($>))
import Data.List (elemIndex)
import Web.Minion
import Web.Minion.Auth.Basic
import Web.Minion.Error (statusOf, unauthorized)

type Env = [BasicAuth]
type M = ReaderT Env IO

app :: IO (ApplicationM IO)
app = do
  let users = [BasicAuth "alice" "123", BasicAuth "bob" "312", BasicAuth "admin" "admin"]
  pure $ \req resp -> runReaderT (serve api req resp) users

api :: Router Void M
api = "api" /> "auth" /> "basic" /> myAuth .> handle @(NoBody Ok) GET endpoint
 where
  endpoint (UserId userId) = liftIO do
    putStrLn ("Called by " <> show userId) $> NoBody

newtype UserId = UserId Int

basicAuthSettings :: BasicAuthSettings M UserId
basicAuthSettings =
  BasicAuthSettings \_ ba -> maybe (BadAuth "Unknown user") (Authenticated . UserId) . elemIndex ba <$> ask

myAuth :: ValueCombinator '[] (WithReq M (Auth '[Basic] UserId)) ts M
myAuth = auth @'[Basic] @UserId (pure $ basicAuthSettings :# HNil) \makeError -> \case
  _ -> do
    liftIO $ putStrLn "Unauthorized!"
    throwM $ makeError (statusOf unauthorized) mempty
