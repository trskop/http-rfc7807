-- |
-- Module:      Servant.Server.RFC7807
-- Description: Servant support for RFC7807 style error response messages.
-- Copyright:   (c) 2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Servant support for [RFC7807 — Problem Details for HTTP APIs
-- ](https://tools.ietf.org/html/rfc7807) style response messages.
module Servant.Server.RFC7807
    (
    -- $intro
      rfc7807ServerError

    -- * @ProblemJSON@ Mime Type
    , ProblemJSON

    -- * Re-exported
    --
    -- | When using 'Rfc7807Error' in more complex way, please, depend on
    -- "Network.HTTP.RFC7807" module directly. More information and more
    -- detailed usage example see "Network.HTTP.RFC7807" module doucmentation.
    , Rfc7807Error(..)
    )
  where

import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (Maybe(Just))
import Data.Proxy (Proxy)
import Data.Semigroup ((<>))

import qualified Data.Aeson as Aeson (FromJSON, ToJSON, encode)
import Network.HTTP.Media ((//), (/:), renderHeader)
import Network.HTTP.Types (hContentType)
import Servant.API.ContentTypes
  ( Accept(contentTypes)
  , MimeRender(mimeRender)
  , MimeUnrender(mimeUnrender)
  , contentType
  , eitherDecodeLenient
  )
import Servant.Server

import Network.HTTP.RFC7807 (Rfc7807Error(..), rfc7807Error)

-- | Media type defined by
-- <https://tools.ietf.org/html/rfc7807#section-6.1 RFC7807>:
-- @application/problem+json@
--
-- The way how this mime type is handled is the same as 'JSON'.
data ProblemJSON

-- TODO: This mime type is specifically designed for RFC7807 representation.
-- Should we enforce that in the encoding and decoding?

-- | @application/problem+json; charset=utf-8@
instance Accept ProblemJSON where
    contentTypes _ = ct /: ("charset", "utf-8") :| [ct]
      where
        ct = "application" // "problem+json"

-- | 'Aeson.encode'
instance Aeson.ToJSON a => MimeRender ProblemJSON a where
    mimeRender _ = Aeson.encode

-- | 'eitherDecodeLenient'
instance Aeson.FromJSON a => MimeUnrender ProblemJSON a where
    mimeUnrender _ = eitherDecodeLenient

-- | Construct Servant 'ServerError' with @application/problem+json@ content
-- type and body as described in <https://tools.ietf.org/html/rfc7807 RFC7807>.
--
-- By using Servant abstractions (like 'MimeRender' and 'Accept') we are able
-- to easily integrate with existing code bases.
--
-- === Usage Example
--
-- @
-- data ErrorType
--     = ValidationError
--     -- ...
--
-- instance 'Aeson.ToJSON' ErrorType where
--     'Aeson.toJSON' = \\case
--         ValidationError ->
--              Aeson.String \"/errors#validation-error\"
--
-- {- ... -} = do
--     {- ... -}
--     unless validationSuccessful do
--         throwError $ 'rfc7807ServerError' (Proxy \@'ProblemJSON') 'err400' ValidationError \\e ->
--             e  { 'title' = \"Request failed to pass data validation\"
--                -- ...
--                }
-- @
rfc7807ServerError
    :: (MimeRender ctype body)
    => Proxy ctype
    -- ^ Media type to use when encoding the error response body. This allows
    -- us to select appropriate mime type, e.g. 'JSON' or 'ProblemJSON'.
    -> ServerError
    -- ^ One of Servant error values e.g. 'err400'.
    -> errorType
    -- ^ Value of the 'type_' field (@\"type\"@ in JSON), the only mandatory
    -- parameter for RFC7807 content.
    -> (Rfc7807Error errorType errorInfo context -> body)
    -- ^ Modify the 'Rfc7807Error' type to your hearts desire.
    --
    -- Reason for the return type to be polymorphic (i.e. @body@) is that we
    -- may want to use a newtype to use a different encoding. This still allows
    -- us to use the @'Rfc7807Error' errorType errorInfo context@ type as a
    -- return type if @errorType@, @errorInfo@, and @context@ can be encoded
    -- into JSON.
    -> ServerError
rfc7807ServerError ctype serverError@ServerError{errHTTPCode, errHeaders} t f =
    serverError
        { errBody =
            mimeRender ctype (f (rfc7807Error t){status = Just errHTTPCode})

        , errHeaders = errHeaders
            <>  [ (hContentType, renderHeader (contentType ctype))
                ]
        }

-- $intro
--
-- The main functionality of this module is 'rfc7807ServerError', which allows
-- us to create Servant's 'ServerError' values with RFC7807 style body.
-- Implementation is more abstract than strictly necessary to account for the
-- fact that @application/problem+json@ may not always be the best mime type to
-- use. This is especially true if we are migrating existing error responses.
-- Another benefit of the abstract way it's defined is that we can potentially
-- use different encoding or serialisation libraries.
