-- |
-- Module:      Network.Wai.RFC7807
-- Description: WAI support for RFC7807 style error response messages
-- Copyright:   (c) 2021 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- WAI support for [RFC7807 — Problem Details for HTTP APIs
-- ](https://tools.ietf.org/html/rfc7807) style response messages.
module Network.Wai.RFC7807
    (
    -- $intro
      rfc7807Response

    -- * Usage Examples
    --
    -- $usageExamples

    -- * Re-exported for Convenience
    , JSON
    , ProblemJSON
    )
  where

import Data.Function (($))
import Data.Maybe (Maybe(Just))
import Data.Proxy (Proxy)
import Data.Semigroup ((<>))

import qualified Data.Text.Encoding as Text (decodeUtf8)
import Network.HTTP.Media (renderHeader)
import Network.HTTP.Types
    ( ResponseHeaders
    , Status(Status, statusCode, statusMessage)
    , hContentType
    )
import Network.Wai (Response, responseLBS)
import Servant.API.ContentTypes
    ( JSON
    , MimeRender
    , contentType
    , mimeRender
    )

import Network.HTTP.RFC7807 (Rfc7807Error(..), rfc7807Error)
import Servant.Server.RFC7807 (ProblemJSON)


-- | Construct WAI `Response` with RFC7807 style body.
--
-- We are reusing some of the Servant machinery in the same way as
-- "Servant.Server.RFC7807" does.
rfc7807Response
    :: forall body ctype errorType errorInfo context
    .  (MimeRender ctype body)
    => Proxy ctype
    -- ^ Media type to use when encoding the error response body. This allows
    -- us to select appropriate mime type, e.g. `Servant.API.ContentTypes.JSON`
    -- or `Servant.Server.RFC7807.ProblemJSON`.
    --
    -- See module "Servant.Server.RFC7807" module for more information as we
    -- are piggybacking on Servant implementation.
    -> Status
    -- ^ One of HTTP status codes, e.g. `Network.HTTP.Types.notFound404`. See
    -- module "Network.HTTP.Types" of [@http-types@
    -- ](https://hackage.haskell.org/package/http-types] package for more
    -- information.
    -> ResponseHeaders
    -- ^ Additional response headers; be aware that @Content-Type@ header will
    -- be set based on the value of @Proxy ctype@ argument and doesn't need to
    -- be set.
    -> errorType
    -- ^ Value of the @type_@ field of 'Rfc7807Error' (@\"type\"@ in JSON), the
    -- only mandatory parameter for RFC7807 content.
    -> (Rfc7807Error errorType errorInfo context -> body)
    -- ^ Modify the `Rfc7807Error` type to your hearts desire.
    --
    -- The @`Rfc7807Error` errorType errorInfo context@ given to this function
    -- will have @type@, @title@, and @status@ set. Values for @title@ and
    -- @status@ are taken from the 'Status' argument. It is highly advised
    -- to modify the @title@ to something more useful.
    --
    -- Reason for the return type to be polymorphic (i.e. @body@) is that we
    -- may want to use a newtype to use a different encoding. This still allows
    -- us to use the @`Rfc7807Error` errorType errorInfo context@ type as a
    -- return type if @errorType@, @errorInfo@, and @context@ can be encoded
    -- into JSON. In other words, `Data.Function.id` is a valid fit.
    -> Response
rfc7807Response
  ctype
  status@Status{statusCode, statusMessage}
  extraHeaders
  errorType
  f =
      responseLBS status headers body
  where
    headers =
        extraHeaders <> [(hContentType, renderHeader (contentType ctype))]

    body = mimeRender ctype $ f (rfc7807Error errorType)
        { status = Just statusCode
        , title = Just (Text.decodeUtf8 statusMessage)
        }

-- $intro
--
-- The main functionality of this module is `rfc7807Error`, which allows us to
-- create WAI `Response` values with RFC7807 style body. Implementation is more
-- abstract than strictly necessary to account for the fact that
-- @application/problem+json@ may not always be the best mime type to use. This
-- is especially true if we are migrating existing error responses. Another
-- benefit of the abstract way it's defined is that we can potentially use
-- different encoding or serialisation libraries.
--
-- Servant machinery for encoding body using correct serialisation and setting
-- the value of @Content-Type@ header is reused. See "Servant.Server.RFC7807"
-- module for details.
--
-- If you're interested in using this module right away then jump straight to
-- [Usage Examples section](#usage-examples).

-- $usageExamples
--
-- #usage-examples#
--
-- __TODO__
--
