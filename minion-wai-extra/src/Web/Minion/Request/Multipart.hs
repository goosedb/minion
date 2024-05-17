module Web.Minion.Request.Multipart (
  Multipart (..),
  multipartBody,
  Backend (..),
  Tmp,
  Mem,
  MultipartData (..),
  FromMultipart (..),
  MultipartM,
  getParam,
  lookupParam,
  getFile,
  lookupFile,
  Wai.File,
  Wai.Param,
) where

import Data.ByteString.Lazy qualified as Bytes.Lazy
import Data.Kind (Type)
import Network.Wai.Parse qualified as Wai

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Except (Except, except, runExcept)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Resource
import Data.ByteString qualified as Bytes
import Data.String.Conversions (ConvertibleStrings (..))
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encode
import Network.HTTP.Types qualified as Http
import Web.Minion.Args (WithReq)
import Web.Minion.Introspect qualified as I
import Web.Minion.Request (IsRequest (..))
import Web.Minion.Router

data Tmp
data Mem

newtype Multipart backend a = Multipart a

instance IsRequest (Multipart backend a) where
  type RequestValue (Multipart backend a) = a
  getRequestValue (Multipart a) = a

type MultipartM backend = ReaderT (MultipartData backend) (Except Text)

class (MonadIO m) => Backend m backend where
  type BackendFile backend :: Type
  waiBackend :: m (Wai.BackEnd (BackendFile backend))

instance (MonadResource m) => Backend m Tmp where
  type BackendFile Tmp = FilePath
  waiBackend = liftResourceT do
    Wai.tempFileBackEnd <$> getInternalState

instance (MonadIO m) => Backend m Mem where
  type BackendFile Mem = Bytes.Lazy.ByteString
  waiBackend = pure Wai.lbsBackEnd

data MultipartData backend = MultipartData
  { params :: [Wai.Param]
  , files :: [Wai.File (BackendFile backend)]
  }

class FromMultipart backend a where
  fromMultipart :: MultipartM backend a

instance FromMultipart backend (MultipartData backend) where
  fromMultipart = ask

{- | Extracts multipart data from request body

@
... /> 'multipartBody' \@'Tmp' @Foo .> ...
@
-}
multipartBody ::
  forall backend r m i ts.
  (I.Introspection i I.Request (Multipart backend r)) =>
  (MonadThrow m) =>
  (FromMultipart backend r) =>
  (Backend m backend) =>
  -- | .
  ValueCombinator i (WithReq m (Multipart backend r)) ts m
multipartBody = Request \makeError req -> do
  backend <- (waiBackend @m @backend)
  (params, files) <- liftIO $ Wai.parseRequestBody backend req
  case runExcept $ runReaderT (fromMultipart @backend @r) MultipartData{..} of
    Left e -> throwM $ makeError req Http.status400 (convertString e)
    Right v -> pure $ Multipart v

{- |
@
instance 'FromMultipart' 'Tmp' MyData where
  'fromMultipart' = do
    param1 <- 'getParam' "param1"
    param2 <- 'getParam' "param2"
    pure MyData {..}
@
-}
getParam :: Bytes.ByteString -> MultipartM backend Bytes.ByteString
getParam a =
  ask
    >>= lift
      . except
      . maybe (Left $ ("Param not found: " <>) $ Text.Encode.decodeUtf8 a) Right
      . lookup a
      . params

{- |
@
instance 'FromMultipart' 'Tmp' MyData where
  'fromMultipart' = do
    param1 <- 'getParam' "param1"
    param2 <- 'getParam' "param2"
    pure MyData {..}
@
-}
lookupParam :: Bytes.ByteString -> MultipartM backend (Maybe Bytes.ByteString)
lookupParam a =
  ask
    >>= lift
      . except
      . Right
      . lookup a
      . params

{- |
@
instance 'FromMultipart' 'Tmp' MyData where
  'fromMultipart' = do
    file1 <- 'lookupFile' "file1"
    file2 <- 'lookupFile' "file2"
    pure MyData {..}
@
-}
lookupFile :: Bytes.ByteString -> MultipartM backend (Maybe (Wai.FileInfo (BackendFile backend)))
lookupFile a =
  ask
    >>= lift
      . except
      . Right
      . lookup a
      . files

{- |
@
instance 'FromMultipart' 'Tmp' MyData where
  'fromMultipart' = do
    file1 <- 'getFile' "file1"
    file2 <- 'getFile' "file2"
    pure MyData {..}
@
-}
getFile :: Bytes.ByteString -> MultipartM backend (Wai.FileInfo (BackendFile backend))
getFile a =
  ask
    >>= lift
      . except
      . maybe (Left $ ("File not found: " <>) $ Text.Encode.decodeUtf8 a) Right
      . lookup a
      . files
