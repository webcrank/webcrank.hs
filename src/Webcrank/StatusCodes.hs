module Webcrank.StatusCodes where

import Webcrank.StatusCode

-- | Defined in [RFC2616]
continue :: StatusCode
continue = StatusCode 100

-- | Defined in [RFC2616]
switchingProtocols :: StatusCode
switchingProtocols = StatusCode 101

-- | Defined in [RFC2518]
processing :: StatusCode
processing = StatusCode 102

-- | Defined in [RFC2616]
ok :: StatusCode
ok = StatusCode 200

-- | Defined in [RFC2616]
created :: StatusCode
created = StatusCode 201

-- | Defined in [RFC2616]
accepted :: StatusCode
accepted = StatusCode 202

-- | Defined in [RFC2616]
nonAuthoritativeInformation :: StatusCode
nonAuthoritativeInformation = StatusCode 203

-- | Defined in [RFC2616]
noContent :: StatusCode
noContent = StatusCode 204

-- | Defined in [RFC2616]
resetContent :: StatusCode
resetContent = StatusCode 205

-- | Defined in [RFC2616]
partialContent :: StatusCode
partialContent = StatusCode 206

-- | Defined in [RFC4918]
multiStatus :: StatusCode
multiStatus = StatusCode 207

-- | Defined in [RFC5842]
alreadyReported :: StatusCode
alreadyReported = StatusCode 208

-- | Defined in [RFC3229]
imUsed :: StatusCode
imUsed = StatusCode 226

-- | Defined in [RFC2616]
multipleChoices :: StatusCode
multipleChoices = StatusCode 300

-- | Defined in [RFC2616]
movedPermanently :: StatusCode
movedPermanently = StatusCode 301

-- | Defined in [RFC2616]
found :: StatusCode
found = StatusCode 302

-- | Defined in [RFC2616]
seeOther :: StatusCode
seeOther = StatusCode 303

-- | Defined in [RFC2616]
notModified :: StatusCode
notModified = StatusCode 304

-- | Defined in [RFC2616]
useProxy :: StatusCode
useProxy = StatusCode 305

-- | Defined in [RFC2616]
reserved :: StatusCode
reserved = StatusCode 306

-- | Defined in [RFC2616]
temporaryRedirect :: StatusCode
temporaryRedirect = StatusCode 307

-- | Defined in [RFC-reschke-http-status-308-07]
permanentRedirect :: StatusCode
permanentRedirect = StatusCode 308

-- | Defined in [RFC2616]
badRequest :: StatusCode
badRequest = StatusCode 400

-- | Defined in [RFC2616]
unauthorized :: StatusCode
unauthorized = StatusCode 401

-- | Defined in [RFC2616]
paymentRequired :: StatusCode
paymentRequired = StatusCode 402

-- | Defined in [RFC2616]
forbidden :: StatusCode
forbidden = StatusCode 403

-- | Defined in [RFC2616]
notFound :: StatusCode
notFound = StatusCode 404

-- | Defined in [RFC2616]
methodNotAllowed :: StatusCode
methodNotAllowed = StatusCode 405

-- | Defined in [RFC2616]
notAcceptable :: StatusCode
notAcceptable = StatusCode 406

-- | Defined in [RFC2616]
proxyAuthenticationRequired :: StatusCode
proxyAuthenticationRequired = StatusCode 407

-- | Defined in [RFC2616]
requestTimeout :: StatusCode
requestTimeout = StatusCode 408

-- | Defined in [RFC2616]
conflict :: StatusCode
conflict = StatusCode 409

-- | Defined in [RFC2616]
gone :: StatusCode
gone = StatusCode 410

-- | Defined in [RFC2616]
lengthRequired :: StatusCode
lengthRequired = StatusCode 411

-- | Defined in [RFC2616]
preconditionFailed :: StatusCode
preconditionFailed = StatusCode 412

-- | Defined in [RFC2616]
requestEntityTooLarge :: StatusCode
requestEntityTooLarge = StatusCode 413

-- | Defined in [RFC2616]
requestUriTooLong :: StatusCode
requestUriTooLong = StatusCode 414

-- | Defined in [RFC2616]
unsupportedMediaType :: StatusCode
unsupportedMediaType = StatusCode 415

-- | Defined in [RFC2616]
requestedRangeNotSatisfiable :: StatusCode
requestedRangeNotSatisfiable = StatusCode 416

-- | Defined in [RFC2616]
expectationFailed :: StatusCode
expectationFailed = StatusCode 417

-- | Defined in [RFC4918]
unprocessableEntity :: StatusCode
unprocessableEntity = StatusCode 422

-- | Defined in [RFC4918]
locked :: StatusCode
locked = StatusCode 423

-- | Defined in [RFC4918]
failedDependency :: StatusCode
failedDependency = StatusCode 424

-- | Defined in [RFC2817]
upgradeRequired :: StatusCode
upgradeRequired = StatusCode 426

-- | Defined in [RFC6585]
preconditionRequired :: StatusCode
preconditionRequired = StatusCode 428

-- | Defined in [RFC6585]
tooManyRequests :: StatusCode
tooManyRequests = StatusCode 429

-- | Defined in [RFC6585]
requestHeaderFieldsTooLarge :: StatusCode
requestHeaderFieldsTooLarge = StatusCode 431

-- | Defined in [RFC2616]
internalServerError :: StatusCode
internalServerError = StatusCode 500

-- | Defined in [RFC2616]
notImplemented :: StatusCode
notImplemented = StatusCode 501

-- | Defined in [RFC2616]
badGateway :: StatusCode
badGateway = StatusCode 502

-- | Defined in [RFC2616]
serviceUnavailable :: StatusCode
serviceUnavailable = StatusCode 503

-- | Defined in [RFC2616]
gatewayTimeout :: StatusCode
gatewayTimeout = StatusCode 504

-- | Defined in [RFC2616]
httpVersionNotSupported :: StatusCode
httpVersionNotSupported = StatusCode 505

-- | Defined in [RFC2295]
variantAlsoNegotiates :: StatusCode
variantAlsoNegotiates = StatusCode 506

-- | Defined in [RFC4918]
insufficientStorage :: StatusCode
insufficientStorage = StatusCode 507

-- | Defined in [RFC5842]
loopDetected :: StatusCode
loopDetected = StatusCode 508

-- | Defined in [RFC2774]
notExtended :: StatusCode
notExtended = StatusCode 510

-- | Defined in [RFC6585]
networkAuthenticationRequired :: StatusCode
networkAuthenticationRequired = StatusCode 511
