{-# LANGUAGE CPP #-}
-- |
-- Module:      Servant.Server.RFC7807
-- Description: Servant support for RFC7807 style error response messages
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

    -- * Mime Type @application\/problem+json@
    , ProblemJSON

    -- * Usage Examples
    --
    -- $usageExamples

    -- ** Direct Use Example
    --
    -- $directUseExample

    -- * Re-exported
    --
    -- | When using 'Rfc7807Error' in more complex way, please, depend on
    -- "Network.HTTP.RFC7807" module directly. More information and more
    -- detailed usage examples can be found in "Network.HTTP.RFC7807" module
    -- documentation.
    , Rfc7807Error(..)

#if !MIN_VERSION_servant_server(0,16,0)
    -- * Servant 0.15 Compatibility
    --
    -- | In @servant-server@ version 0.16 'ServantErr' was renamed to
    -- 'ServerError'. This package provides compatiblity for that version, but
    -- it may be dropped in the near future.
    --
    -- For more information see [@servant-server-1.16 ChangeLog
    -- ](https://hackage.haskell.org/package/servant-server-0.16/changelog).
    , ServerError
#endif
    )
  where

import Data.Function (($))
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (Maybe(Just))
import Data.Proxy (Proxy)
import Data.Semigroup ((<>))
import Data.String (fromString)

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
-- The way how this mime type is handled is the same as
-- 'Servant.API.ContentTypes.JSON'.
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

#if !MIN_VERSION_servant_server(0,16,0)
-- | Compatibility with newer @servant-server@ versions as 'ServantErr' was
-- renamed to 'ServerError' in version 0.16.
type ServerError = ServantErr
#endif

-- | Construct Servant 'ServerError' with RFC7807 style response body.
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
--     toJSON = \\case
--         ValidationError ->
--              'Aeson.String' \"/errors#validation-error\"
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
    :: forall body ctype errorType errorInfo context
    .  (MimeRender ctype body)
    => Proxy ctype
    -- ^ Media type to use when encoding the error response body. This allows
    -- us to select appropriate mime type, e.g. 'Servant.API.ContentTypes.JSON'
    -- or 'ProblemJSON'.
    -> ServerError
    -- ^ One of Servant error values e.g. 'err400'.
    -> errorType
    -- ^ Value of the 'type_' field (@\"type\"@ in JSON), the only mandatory
    -- parameter for RFC7807 content.
    -> (Rfc7807Error errorType errorInfo context -> body)
    -- ^ Modify the 'Rfc7807Error' type to your hearts desire.
    --
    -- The @'Rfc7807Error' errorType errorInfo context@ given to this function
    -- will have @type@, @title@, and @status@ set. Values for @title@ and
    -- @status@ are taken from Servant's 'ServerError'. It is highly advised
    -- to modify the @title@ to something more useful.
    --
    -- Reason for the return type to be polymorphic (i.e. @body@) is that we
    -- may want to use a newtype to use a different encoding. This still allows
    -- us to use the @'Rfc7807Error' errorType errorInfo context@ type as a
    -- return type if @errorType@, @errorInfo@, and @context@ can be encoded
    -- into JSON. In other words, 'Data.Function.id' is a valid fit.
    -> ServerError
rfc7807ServerError
  ctype
#if MIN_VERSION_servant_server(0,16,0)
  serverError@ServerError{errHTTPCode, errHeaders, errReasonPhrase}
#else
  serverError@ServantErr{errHTTPCode, errHeaders, errReasonPhrase}
#endif
  errorType'
  f =
    serverError
        { errBody =
            mimeRender ctype $ f (rfc7807Error errorType')
                { status = Just errHTTPCode
                , title = Just (fromString errReasonPhrase)
                }

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
--
-- If you're interested in using this module right away then jump straight to
-- [Usage Examples section](#usage-examples).

-- $usageExamples
--
-- #usage-examples#
--
-- These examples focus on usage of 'rfc7807ServerError', to see examples more
-- related to the 'Rfc7807Error' messages go to "Network.HTTP.RFC7807" module.
--
-- Haskell\/GHC language extensions being used in the examples:
--
-- * @RecordWildCards@ and @NamedFieldPuns@ — please read this great article
--   if you're not familiar with these extensions: [The Power of RecordWildCards
--   by Dmitrii Kovanikov](https://kodimensional.dev/recordwildcards).
--
-- * @OverloadedStrings@ — allows us to define string literals for types like
--   'Text' without needing to manually pack\/convert 'String' values. See
--   [GHC User's Guide — Overloaded string literals
--   ](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html#overloaded-string-literals)
--   for more information.

-- $directUseExample
--
-- This example is intended to illustrate how we can start producing RFC7807
-- style responses without too much fuss. No complex abstractions, no custom
-- wrappers for 'Rfc7807Error', no custom serialisation, and no extra @context@.
--
-- @
-- -- | Servant definition of an endpoint.
-- type SomeEndpoint = {- ... -}
--
-- -- | This code is not complex enough to actually need to be in a function,
-- -- but it makes some things more obious and easier to change.
-- badRequest
--     :: ( MonadError 'ServerError' m
--        , 'Aeson.ToJSON' errorType
--        , 'Aeson.ToJSON' errorInfo
--        )
--     => errorType
--     -> ( 'Rfc7807Error' errorType errorInfo ()
--        -> 'Rfc7807Error' errorType errorInfo ()
--        )
--     -> m a
-- badRequest errorType =
--     throwError . 'rfc7807ServerError' (Proxy \@'ProblemJSON') 'err400' errorType
--
-- -- | See "Network.HTTP.RFC7807" module for more information and examples on
-- -- how to use and define data types to be used for @errorType@.
-- data ErrorType
--     = ValidationError
--     -- ...
--
-- instance 'Aeson.ToJSON' ErrorType where
--     toJSON = \\case
--         ValidationError ->
--              'Data.Aeson.String' \"/errors#some-endpoint-validation-error\"
--
-- someHandler :: 'ServerT' SomeEndpoint m
-- someHandler request = do
--     response <- doTheEndpointStuffBasedOn request
--
--     case response of
--         Success r ->
--             pure r
--
--         InvalidRequest error_@DataValidationFailed ->
--             badRequest ValidationError \\e -> e
--                 { title = \"Request data validation failed\"
--                 , detail = \"One or more members of request's 'data' field\\
--                     \\ failed validation, see 'error' field\"
--
--                 -- If we've used something like \@{\"error\": TheError}\@
--                 -- before switching to RFC7807 then this will be backward
--                 -- compatible. We can also play with the serialisation if we
--                 -- need to preserve backward compatibility. It won't work
--                 -- all the time though.
--                 --
--                 -- Huge downside of this approach is that the error is
--                 -- directly serialised into JSON. API contract can easily be
--                 -- affected by changes that seem unrelated. Please, consider
--                 -- having a separate data type for this purpose or use JSON
--                 -- combinators.
--                 , error_
--                 }
--
--         {- ... -}
-- @
