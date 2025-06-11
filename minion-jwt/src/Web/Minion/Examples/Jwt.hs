module Web.Minion.Examples.Jwt (app) where

import Control.Monad (forM_)
import Control.Monad.Reader (MonadIO (liftIO), ReaderT (runReaderT), asks)
import Crypto.JOSE (JWK, bestJWSAlg, fromOctets, newJWSHeader, runJOSE)
import Crypto.JWT (JWTError, encodeCompact, signJWT)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy qualified as Bytes.Lazy
import Data.Functor (($>))
import Data.Text.Encoding qualified
import Data.Text.IO qualified
import GHC.Generics (Generic)
import Network.HTTP.Types.Status qualified as Http
import System.Environment (getArgs)
import Web.Minion
import Web.Minion.Auth.Jwt

type M = ReaderT Env IO

newtype JwtUserInfo = JwtUserInfo {userId :: UserId}
  deriving (Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype UserId = UserId Int
  deriving newtype (FromJSON, Show, Read, ToJSON)

newtype Env = Env
  {authCtx :: HList '[JwtAuthSettings M JwtUserInfo UserId]}

app :: IO (ApplicationM IO)
app = do
  showJwts
  pure $ \req resp ->
    runReaderT (serve api req resp) (Env $ jwtSettings :# HNil)

api :: Router Void M
api = "api" /> "auth" /> myAuth .> handle GET authEndpoint

authEndpoint :: UserId -> ReaderT Env IO NoBody
authEndpoint userId = liftIO (putStrLn $ "User " <> show userId) $> NoBody

myAuth :: ValueCombinator '[] (WithReq M (Auth '[Bearer JwtUserInfo] UserId)) ts M
myAuth = auth @'[Bearer JwtUserInfo] @UserId (asks authCtx) \makeError -> \case
  _ -> throwM $ makeError Http.status401 mempty

jwtSettings :: JwtAuthSettings M JwtUserInfo UserId
jwtSettings = defaultJwtAuthSettings (pure myJwk) (const True) do
  const (pure . either (const BadAuth) (\JwtPayload{payload = JwtUserInfo{..}} -> Authenticated userId))

myJwk :: JWK
myJwk = fromOctets @Bytes.Lazy.ByteString "really secret and long enough key"

showJwts :: IO ()
showJwts = do
  userIds <- map (read @UserId) <$> getArgs
  forM_ userIds \userId -> do
    Right jwt <- runJOSE @JWTError do
      alg <- bestJWSAlg myJwk
      signJWT myJwk (newJWSHeader ((), alg)) (JwtUserInfo userId)
    let jwtTxt = Data.Text.Encoding.decodeUtf8 . Bytes.Lazy.toStrict $ encodeCompact jwt
    putStr (show userId <> ": ") >> Data.Text.IO.putStrLn jwtTxt
