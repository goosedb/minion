{-# LANGUAGE DeriveFunctor #-}

module Web.Minion.Response.Status where

import Network.HTTP.Types
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Web.Minion hiding (status)

newtype WithStatus status a = WithStatus
  {body :: a}
  deriving (Functor)

instance (ToResponse m a, Monad m, IsStatus status) => ToResponse m (WithStatus status a) where
  toResponse accept WithStatus{..} =
    Wai.mapResponseStatus (const $ status @status)
      <$> toResponse accept body

instance (CanRespond a) => CanRespond (WithStatus status a) where
  canRespond = canRespond @a

class IsStatus status where
  status :: Http.Status

data Continue

instance IsStatus Continue where
  status = status100

data SwitchingProtocols
instance IsStatus SwitchingProtocols where
  status = status101

data OK
instance IsStatus OK where
  status = status200

data Created
instance IsStatus Created where
  status = status201

data Accepted
instance IsStatus Accepted where
  status = status202

data NonAuthoritativeInformation
instance IsStatus NonAuthoritativeInformation where
  status = status203

data NoContent
instance IsStatus NoContent where
  status = status204

data ResetContent
instance IsStatus ResetContent where
  status = status205

data PartialContent
instance IsStatus PartialContent where
  status = status206

data MultipleChoices
instance IsStatus MultipleChoices where
  status = status300

data MovedPermanently
instance IsStatus MovedPermanently where
  status = status301

data Found
instance IsStatus Found where
  status = status302

data SeeOther
instance IsStatus SeeOther where
  status = status303

data NotModified
instance IsStatus NotModified where
  status = status304

data UseProxy
instance IsStatus UseProxy where
  status = status305

data TemporaryRedirect
instance IsStatus TemporaryRedirect where
  status = status307

data PermanentRedirect
instance IsStatus PermanentRedirect where
  status = status308

data BadRequest
instance IsStatus BadRequest where
  status = status400

data Unauthorized
instance IsStatus Unauthorized where
  status = status401

data PaymentRequired
instance IsStatus PaymentRequired where
  status = status402

data Forbidden
instance IsStatus Forbidden where
  status = status403

data NotFound
instance IsStatus NotFound where
  status = status404

data MethodNotAllowed
instance IsStatus MethodNotAllowed where
  status = status405

data NotAcceptable
instance IsStatus NotAcceptable where
  status = status406

data ProxyAuthenticationRequired
instance IsStatus ProxyAuthenticationRequired where
  status = status407

data RequestTimeout
instance IsStatus RequestTimeout where
  status = status408

data Conflict
instance IsStatus Conflict where
  status = status409

data Gone
instance IsStatus Gone where
  status = status410

data LengthRequired
instance IsStatus LengthRequired where
  status = status411

data PreconditionFailed
instance IsStatus PreconditionFailed where
  status = status412

data PayloadTooLarge
instance IsStatus PayloadTooLarge where
  status = status413

data URITooLong
instance IsStatus URITooLong where
  status = status414

data UnsupportedMediaType
instance IsStatus UnsupportedMediaType where
  status = status415

data RangeNotSatisfiable
instance IsStatus RangeNotSatisfiable where
  status = status416

data ExpectationFailed
instance IsStatus ExpectationFailed where
  status = status417

data ImATeapot
instance IsStatus ImATeapot where
  status = status418

data UnprocessableEntity
instance IsStatus UnprocessableEntity where
  status = status422

data PreconditionRequired
instance IsStatus PreconditionRequired where
  status = status428

data TooManyRequests
instance IsStatus TooManyRequests where
  status = status429

data RequestHeaderFieldsTooLarge
instance IsStatus RequestHeaderFieldsTooLarge where
  status = status431

data InternalServerError
instance IsStatus InternalServerError where
  status = status500

data NotImplemented
instance IsStatus NotImplemented where
  status = status501

data BadGateway
instance IsStatus BadGateway where
  status = status502

data ServiceUnavailable
instance IsStatus ServiceUnavailable where
  status = status503

data GatewayTimeout
instance IsStatus GatewayTimeout where
  status = status504

data HTTPVersionNotSupported
instance IsStatus HTTPVersionNotSupported where
  status = status505

data NetworkAuthenticationRequired
instance IsStatus NetworkAuthenticationRequired where
  status = status511
