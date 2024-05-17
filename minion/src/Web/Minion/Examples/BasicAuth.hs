module Web.Minion.Examples.BasicAuth (app) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Data.IORef (IORef, newIORef, readIORef)
import Data.List (elemIndex)
import Web.Minion
import Web.Minion.Auth.Basic
import Web.Minion.Error (codeOf, unauthorized)

type Env = IORef [BasicAuth]
type M = ReaderT Env IO

app :: IO (ApplicationM IO)
app = do
  users <-
    newIORef
      [ BasicAuth "alice" "123"
      , BasicAuth "bob" "312"
      , BasicAuth "admin" "admin"
      ]
  pure $ \req resp -> runReaderT (serve api req resp) users

api :: Router Void M
api = "api" /> "auth" /> "basic" /> myAuth .> handle @NoBody GET endpoint
 where
  endpoint (UserId userId) = liftIO do
    putStrLn $ "Called " <> show userId
    pure NoBody

newtype UserId = UserId Int

basicAuthSettings :: HList '[BasicAuthSettings M UserId]
basicAuthSettings =
  BasicAuthSettings
    { check = \_ ba -> do
        usersRef <- ask
        users <- liftIO $ readIORef usersRef
        pure $ maybe BadAuth (Authenticated . UserId) $ elemIndex ba users
    }
    :# HNil

myAuth :: ValueCombinator Void (WithReq M (Auth '[Basic] UserId)) ts M
myAuth = auth @'[Basic] @UserId (pure basicAuthSettings) \makeError -> \case
  _ -> do
    liftIO $ putStrLn "Unauthozied!"
    throwM $ makeError (codeOf unauthorized) mempty
