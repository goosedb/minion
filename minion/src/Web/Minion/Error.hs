module Web.Minion.Error (MonadThrow (..), module Web.Minion.Error) where

import Control.Exception
import Control.Monad.Catch (MonadThrow (..))
import Data.ByteString qualified as Bytes
import Data.ByteString.Lazy qualified as Bytes.Lazy
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai

newtype NoMatch = NoMatch (Maybe ServerError)
  deriving stock (Show)
  deriving anyclass (Exception)

type ErrorBuilder = Wai.Request -> Http.Status -> Bytes.Lazy.ByteString -> ServerError

type TextToError = Bytes.Lazy.ByteString -> ServerError

data SomethingWentWrong = SomethingWentWrong
  deriving (Show, Exception)

data ErrorBuilders = ErrorBuilders
  { headerErrorBuilder :: ErrorBuilder
  , queryParamsErrorBuilder :: ErrorBuilder
  , captureErrorBuilder :: ErrorBuilder
  , bodyErrorBuilder :: ErrorBuilder
  }

data ServerError = ServerError
  { status :: Http.Status
  , headers :: [Http.Header]
  , body :: Bytes.Lazy.ByteString
  }
  deriving (Show, Exception)

statusOf :: ServerError -> Http.Status
statusOf ServerError{..} = status

codeOf :: ServerError -> Int
codeOf ServerError{..} = Http.statusCode status

redirect :: (MonadThrow m) => Bytes.ByteString -> m a
redirect url = throwM $ found{headers = [("Location", url)]}

err300 :: ServerError
err300 = ServerError Http.status300 [] mempty

err301 :: ServerError
err301 = ServerError Http.status301 [] mempty

err302 :: ServerError
err302 = ServerError Http.status302 [] mempty

err303 :: ServerError
err303 = ServerError Http.status303 [] mempty

err304 :: ServerError
err304 = ServerError Http.status304 [] mempty

err305 :: ServerError
err305 = ServerError Http.status305 [] mempty

err307 :: ServerError
err307 = ServerError Http.status307 [] mempty

err400 :: ServerError
err400 = ServerError Http.status400 [] mempty

err401 :: ServerError
err401 = ServerError Http.status401 [] mempty

err402 :: ServerError
err402 = ServerError Http.status402 [] mempty

err403 :: ServerError
err403 = ServerError Http.status403 [] mempty

err404 :: ServerError
err404 = ServerError Http.status404 [] mempty

err405 :: ServerError
err405 = ServerError Http.status405 [] mempty

err406 :: ServerError
err406 = ServerError Http.status406 [] mempty

err407 :: ServerError
err407 = ServerError Http.status407 [] mempty

err409 :: ServerError
err409 = ServerError Http.status409 [] mempty

err410 :: ServerError
err410 = ServerError Http.status410 [] mempty

err411 :: ServerError
err411 = ServerError Http.status411 [] mempty

err412 :: ServerError
err412 = ServerError Http.status412 [] mempty

err413 :: ServerError
err413 = ServerError Http.status413 [] mempty

err414 :: ServerError
err414 = ServerError Http.status414 [] mempty

err415 :: ServerError
err415 = ServerError Http.status415 [] mempty

err416 :: ServerError
err416 = ServerError Http.status416 [] mempty

err417 :: ServerError
err417 = ServerError Http.status417 [] mempty

err418 :: ServerError
err418 = ServerError Http.status418 [] mempty

err422 :: ServerError
err422 = ServerError Http.status422 [] mempty

err500 :: ServerError
err500 = ServerError Http.status500 [] mempty

err501 :: ServerError
err501 = ServerError Http.status501 [] mempty

err502 :: ServerError
err502 = ServerError Http.status502 [] mempty

err503 :: ServerError
err503 = ServerError Http.status503 [] mempty

err504 :: ServerError
err504 = ServerError Http.status504 [] mempty

err505 :: ServerError
err505 = ServerError Http.status505 [] mempty

multipleChoices :: ServerError
multipleChoices = err300

movedPermanently :: ServerError
movedPermanently = err301

found :: ServerError
found = err302

seeOther :: ServerError
seeOther = err303

notModified :: ServerError
notModified = err304

useProxy :: ServerError
useProxy = err305

temporaryRedirect :: ServerError
temporaryRedirect = err307

badRequest :: ServerError
badRequest = err400

unauthorized :: ServerError
unauthorized = err401

paymentRequired :: ServerError
paymentRequired = err402

forbidden :: ServerError
forbidden = err403

notFound :: ServerError
notFound = err404

methodNotAllowed :: ServerError
methodNotAllowed = err405

notAcceptable :: ServerError
notAcceptable = err406

proxyAuthenticationRequired :: ServerError
proxyAuthenticationRequired = err407

conflict :: ServerError
conflict = err409

gone :: ServerError
gone = err410

lengthRequired :: ServerError
lengthRequired = err411

preconditionFailed :: ServerError
preconditionFailed = err412

requestEntityTooLarge :: ServerError
requestEntityTooLarge = err413

requestURITooLong :: ServerError
requestURITooLong = err414

unsupportedMediaType :: ServerError
unsupportedMediaType = err415

requestedRangeNotSatisfiable :: ServerError
requestedRangeNotSatisfiable = err416

expectationFailed :: ServerError
expectationFailed = err417

teapot :: ServerError
teapot = err418

unprocessableEntity :: ServerError
unprocessableEntity = err422

internalServerError :: ServerError
internalServerError = err500

notImplemented :: ServerError
notImplemented = err501

badGateway :: ServerError
badGateway = err502

serviceUnavailable :: ServerError
serviceUnavailable = err503

gatewayTimeout :: ServerError
gatewayTimeout = err504

httpVersionNotSupported :: ServerError
httpVersionNotSupported = err505
