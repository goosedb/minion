module Web.Minion.Response.Status where

import Network.HTTP.Types
import Network.HTTP.Types qualified as Http

class IsStatus httpStatus where
  httpStatus :: Http.Status

data Continue

instance IsStatus Continue where
  httpStatus = status100

data SwitchingProtocols
instance IsStatus SwitchingProtocols where
  httpStatus = status101

data OK
instance IsStatus OK where
  httpStatus = status200

data Created
instance IsStatus Created where
  httpStatus = status201

data Accepted
instance IsStatus Accepted where
  httpStatus = status202

data NonAuthoritativeInformation
instance IsStatus NonAuthoritativeInformation where
  httpStatus = status203

data NoContent
instance IsStatus NoContent where
  httpStatus = status204

data ResetContent
instance IsStatus ResetContent where
  httpStatus = status205

data PartialContent
instance IsStatus PartialContent where
  httpStatus = status206

data MultipleChoices
instance IsStatus MultipleChoices where
  httpStatus = status300

data MovedPermanently
instance IsStatus MovedPermanently where
  httpStatus = status301

data Found
instance IsStatus Found where
  httpStatus = status302

data SeeOther
instance IsStatus SeeOther where
  httpStatus = status303

data NotModified
instance IsStatus NotModified where
  httpStatus = status304

data UseProxy
instance IsStatus UseProxy where
  httpStatus = status305

data TemporaryRedirect
instance IsStatus TemporaryRedirect where
  httpStatus = status307

data PermanentRedirect
instance IsStatus PermanentRedirect where
  httpStatus = status308

data BadRequest
instance IsStatus BadRequest where
  httpStatus = status400

data Unauthorized
instance IsStatus Unauthorized where
  httpStatus = status401

data PaymentRequired
instance IsStatus PaymentRequired where
  httpStatus = status402

data Forbidden
instance IsStatus Forbidden where
  httpStatus = status403

data NotFound
instance IsStatus NotFound where
  httpStatus = status404

data MethodNotAllowed
instance IsStatus MethodNotAllowed where
  httpStatus = status405

data NotAcceptable
instance IsStatus NotAcceptable where
  httpStatus = status406

data ProxyAuthenticationRequired
instance IsStatus ProxyAuthenticationRequired where
  httpStatus = status407

data RequestTimeout
instance IsStatus RequestTimeout where
  httpStatus = status408

data Conflict
instance IsStatus Conflict where
  httpStatus = status409

data Gone
instance IsStatus Gone where
  httpStatus = status410

data LengthRequired
instance IsStatus LengthRequired where
  httpStatus = status411

data PreconditionFailed
instance IsStatus PreconditionFailed where
  httpStatus = status412

data PayloadTooLarge
instance IsStatus PayloadTooLarge where
  httpStatus = status413

data URITooLong
instance IsStatus URITooLong where
  httpStatus = status414

data UnsupportedMediaType
instance IsStatus UnsupportedMediaType where
  httpStatus = status415

data RangeNotSatisfiable
instance IsStatus RangeNotSatisfiable where
  httpStatus = status416

data ExpectationFailed
instance IsStatus ExpectationFailed where
  httpStatus = status417

data ImATeapot
instance IsStatus ImATeapot where
  httpStatus = status418

data UnprocessableEntity
instance IsStatus UnprocessableEntity where
  httpStatus = status422

data PreconditionRequired
instance IsStatus PreconditionRequired where
  httpStatus = status428

data TooManyRequests
instance IsStatus TooManyRequests where
  httpStatus = status429

data RequestHeaderFieldsTooLarge
instance IsStatus RequestHeaderFieldsTooLarge where
  httpStatus = status431

data InternalServerError
instance IsStatus InternalServerError where
  httpStatus = status500

data NotImplemented
instance IsStatus NotImplemented where
  httpStatus = status501

data BadGateway
instance IsStatus BadGateway where
  httpStatus = status502

data ServiceUnavailable
instance IsStatus ServiceUnavailable where
  httpStatus = status503

data GatewayTimeout
instance IsStatus GatewayTimeout where
  httpStatus = status504

data HTTPVersionNotSupported
instance IsStatus HTTPVersionNotSupported where
  httpStatus = status505

data NetworkAuthenticationRequired
instance IsStatus NetworkAuthenticationRequired where
  httpStatus = status511
