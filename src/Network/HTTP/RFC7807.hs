-- |
-- Module:      Network.HTTP.RFC7807
-- Description: RFC7807 style response messages.
-- Copyright:   (c) 2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- [RFC7807 — Problem Details for HTTP APIs](https://tools.ietf.org/html/rfc7807)
-- style response messages.
module Network.HTTP.RFC7807
    (
      Rfc7807Error(..)
    , rfc7807Error

    -- * Encoding and Decoding
    --
    -- | Useful for defining your own encoding\/decoding instances.
    , ExtensionField(..)
    , EncodingOptions(..)
    , defaultEncodingOptions
    , toKeyValue
    , parseJSON

    -- * Usage Examples
    --
    -- $usageExamples
    )
  where

import Control.Applicative (pure)
import Data.Bool (Bool(False, True), (||), not, otherwise)
import Data.Eq (Eq)
import Data.Function (($), const)
import Data.Int (Int)
import Data.Maybe (Maybe(Nothing), isJust)
import Data.Monoid (Monoid, mconcat, mempty)
import Data.Proxy (Proxy(Proxy))
import Data.String (String)
import Data.Typeable (Typeable, typeRep)
import GHC.Generics (Generic)
import Text.Show (Show, show)

import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson (Parser)
import Data.Text (Text)


-- | Based on [RFC7807](https://tools.ietf.org/html/rfc7807) with few
-- additional fields (@'error_' :: errorInfo@ and @'context' :: context@).
--
-- Meaning of individual type parameters:
--
-- [@errorType@]: Represents an URI reference. Easiest to start with is just
--   using 'Text' type; simplest and most extensible is defining an enum with a
--   'Aeson.ToJSON':
--   @
--   data ErrorType
--       = DocumentNotFound
--       | DocumentAccessForbidden
--       {- ... -}
--
--   instance 'Aeson.ToJSON' ErrorType where
--       toJSON = \\case
--           DocumentNotFound ->
--               mkUrl \"document-not-found\"
--           DocumentAccessForbidden ->
--               mkUrl \"document-access-forbidden\"
--           {- ... -}
--         where
--           mkUrl t = 'Aeson.Text' (\"https://example.com/docs/error#\" <> t)
--   @
--
-- [@errorInfo@]: Not defined by RFC7807. This type is intended to provide a
--   different representation of the error. This is very useful when you're
--   retrofitting RFC7807 style messages into an existing error reporting.
--   Another common use case is when client needs to understand the error
--   response. For example, form validation errors that need to be displayed in
--   context of the element that failed validation. If you're not using this
--   you can set the type to @()@.
--
-- [@context@]: Not defined by RFC3986. This type is intended to provide more
--   details\/context to what has happened. For example, IDs of entities that
--   were involved. If you're not using this you can set the type to @()@.
data Rfc7807Error errorType errorInfo context = Rfc7807Error
    { type_ :: errorType
    -- ^ (required) A URI reference
    -- <https://tools.ietf.org/html/rfc3986 [RFC3986]> that identifies the
    -- problem type.  This specification encourages that, when dereferenced, it
    -- provide human-readable documentation for the problem type (e.g., using
    -- HTML
    -- <https://tools.ietf.org/html/rfc7807#ref-W3C.REC-html5-20141028 [W3C.REC-html5-20141028]>).
    -- When this member is not present, its value is assumed to be
    -- @\"about:blank\"@.
    --
    -- Consumers MUST use the @\"type\"@ string as the primary identifier for
    -- the problem type; the @\"title\"@ string is advisory and included only
    -- for users who are not aware of the semantics of the URI and do not
    -- have the ability to discover them (e.g., offline log analysis).
    -- Consumers SHOULD NOT automatically dereference the type URI.
    --
    -- Relative URIs are accepted; this means that they must be resolved
    -- relative to the document's base URI, as per
    -- <https://tools.ietf.org/html/rfc3986#section-5 [RFC3986], Section 5>.
    --
    -- In JSON this filed ins named only @\"type\"@.

    , title :: Maybe Text
    -- ^ (optional) A short, human-readable summary of the problem type.  It
    -- SHOULD NOT change from occurrence to occurrence of the problem, except
    -- for purposes of localization (e.g., using proactive content negotiation;
    -- see
    -- <https://tools.ietf.org/html/rfc7231#section-3.4 [RFC7231], Section 3.4>).
    --
    -- Consumers MUST use the @\"type\"@ string as the primary identifier for
    -- the problem type; the @\"title\"@ string is advisory and included only
    -- for users who are not aware of the semantics of the URI and do not
    -- have the ability to discover them (e.g., offline log analysis).
    -- Consumers SHOULD NOT automatically dereference the type URI.

    , status :: Maybe Int
    -- ^ (optional) The HTTP status code
    -- (<https://tools.ietf.org/html/rfc7231#section-6 [RFC7231], Section 6>)
    -- generated by the origin server for this occurrence of the problem.
    --
    -- If present, is only advisory; it conveys the HTTP status code used for
    -- the convenience of the consumer.  Generators MUST use the same status
    -- code in the actual HTTP response, to assure that generic HTTP software
    -- that does not understand this format still behaves correctly.  See
    -- <https://tools.ietf.org/html/rfc7807#section-5 [RFC7807], Section 5> for
    -- further caveats regarding its use.
    --
    -- Consumers can use the status member to determine what the original
    -- status code used by the generator was, in cases where it has been
    -- changed (e.g., by an intermediary or cache), and when message bodies
    -- persist without HTTP information.  Generic HTTP software will still use
    -- the HTTP status code.

    , detail :: Maybe Text
    -- ^ (optional) A human-readable explanation specific to this occurrence of
    -- the problem.
    --
    -- If present, ought to focus on helping the client correct the problem,
    -- rather than giving debugging information.  Consumers SHOULD NOT parse
    -- the "detail" member for information; extensions are more suitable and
    -- less error-prone ways to obtain such information.

    , instance_ :: Maybe Text
    -- ^ A URI reference that identifies the specific occurrence of the
    -- problem.  It may or may not yield further information if dereferenced.
    --
    -- Relative URIs are accepted; this means that they must be resolved
    -- relative to the document's base URI, as per
    -- <https://tools.ietf.org/html/rfc3986#section-5 [RFC3986], Section 5>.
    --
    -- In JSON this filed ins named only @\"instance\"@.

    , error_ :: Maybe errorInfo
    -- ^ (optional, extension) An additional representation of the error.  Lots
    -- of clients detect that the response is an error using simple algorithm
    -- of checking presence of the field @\"error\"@ that has non-@null@ value.
    --
    -- How the field is named in the resulting JSON object is controlled by
    -- 'extensionFieldName', but by default it is @\"error\"@.

    , context :: Maybe context
    -- ^ (optional, extension) Extra information for the purposes of debugging.
    --
    -- How the field is named in the resulting JSON object is controlled by
    -- 'extensionFieldName', but by default it is @\"context\"@.
    }
  deriving stock (Eq, Generic, Show)

-- | Constructor for 'Rfc7807Error' that set's only 'type_' and everything else
-- is set to 'Nothing'.
--
-- Usage example that illustrates how the function is used, not necessarily the
-- best error response you can provide to your client:
--
-- @
-- ('rfc7807Error' \"/errors#not-found\"){'status' = 404}
-- @
rfc7807Error :: errorType -> Rfc7807Error errorType errorInfo context
rfc7807Error type_ = Rfc7807Error
    { type_
    , title = Nothing
    , status = Nothing
    , detail = Nothing
    , instance_ = Nothing
    , error_ = Nothing
    , context = Nothing
    }

-- | Enum representing the extension fields 'error_' and 'context' that are not
-- defined by RFC7807.
--
-- This allows us to reference the field in 'EncodingOptions' and later in
-- 'toKeyValue' and 'parseJSON' without resolving to using 'Text'.
data ExtensionField
    = ErrorField
    -- ^ Represents the name of the 'error_' field of 'Rfc7807Error' data type.
    | ContextField
    -- ^ Represents the name of the 'context' field of 'Rfc7807Error' data type.
  deriving stock (Eq, Generic, Show)

-- {{{ JSON Encoding ----------------------------------------------------------

-- | Encode 'Rfc7807Error' using default 'EncodingOptions':
-- @
-- 'Aeson.toJSON' v = 'Aeson.Object' ('toKeyValue' 'defaultEncodingOptions' v)
-- 'Aeson.toEncoding' v = 'Aeson.pairs' ('toKeyValue' 'defaultEncodingOptions' v)
-- @
instance
    ( Aeson.ToJSON errorType
    , Aeson.ToJSON errorInfo
    , Aeson.ToJSON context
    ) => Aeson.ToJSON (Rfc7807Error errorType errorInfo context)
  where
    toJSON :: Rfc7807Error errorType errorInfo context -> Aeson.Value
    toJSON v = Aeson.Object (toKeyValue defaultEncodingOptions v)

    toEncoding :: Rfc7807Error errorType errorInfo context -> Aeson.Encoding
    toEncoding v = Aeson.pairs (toKeyValue defaultEncodingOptions v)

-- | Parameters that allow us to control certain aspects of how 'Rfc7807Error'
-- is encoded\/decoded to\/from JSON.
data EncodingOptions = EncodingOptions
    { omitNothingFields :: Bool
    -- ^ If set to @True@ (default), record fields of 'Rfc7807Error' with a
    -- 'Nothing' value will be omitted from the resulting object instead of
    -- being represented as @null@.
    --
    -- If set to @False@, the resulting JSON object will include those fields
    -- and the 'Nothing' value will be mapped to @null@ JSON value.
    --
    -- This setting is ignored by 'parse' function as respecting it would mean
    -- that even valid RFC7807 messages would fail to parse.

    , omitExtensionField :: ExtensionField -> Bool
    -- ^ If the function returns @True@ then the specified record field of
    -- 'Rfc7807Error' will be omitted entirely even if it contains
    -- 'Data.Maybe.Just' value.
    --
    -- If the function returns @False@ then the specified record field is
    -- included in the serialised output. However, if the value of that field
    -- is 'Nothing' and 'omitNothingFields' is set to @True@ then the field
    -- will once again be omitted from the resulting JSON object.
    --
    -- This setting can be used in a similar fashion as verbosity level. For
    -- example, we can omit these fields on production and have them enabled
    -- in testing or dev environments.
    --
    -- This setting is respected by 'parse' function, which will ignore
    -- extension fields for which the function returns @True@. Ignored
    -- extension fields will always be set to 'Nothing'.

    , extensionFieldName :: ExtensionField -> Text
    -- Fields 'error_' and 'context' are not defined by RFC7807 and as such
    -- their names may be adjusted depending on our particular needs and
    -- conventions. This function allows exactly that.
    --
    -- This setting is respected by 'parse' function, which will use this
    -- function when searching for extension fields in a JSON object.
    }
  deriving stock (Generic)

-- | Default 'EncodingOptions':
--
-- @
-- defaultEncodingOptions = 'EncodingOptions'
--     { 'omitNothingFields' = True
--     , 'omitExtensionField' = const False
--     }
-- @
defaultEncodingOptions :: EncodingOptions
defaultEncodingOptions = EncodingOptions
    { omitNothingFields = True
    , omitExtensionField = const False
    , extensionFieldName = \case
        ErrorField -> "error"
        ContextField -> "context"
    }

-- | Serialise 'Rfc7807Error' into a key-value pairs. This an abstract way how
-- to support both types of Aeson encodings.
--
-- @
-- 'Aeson.Object' . 'toKeyValue' 'defaultEncodingOptions'
--     ::  ( 'Aeson.ToJSON' errorType
--         , 'Aeson.ToJSON' errorInfo
--         , 'Aeson.ToJSON' context
--         )
--     => 'Rfc7807Error' errorType errorInfo context
--     -> 'Aeson.Value'
--
-- 'Aeson.pairs' . 'toKeyValue' 'defaultEncodingOptions'
--     ::  ( 'Aeson.ToJSON' errorType
--         , 'Aeson.ToJSON' errorInfo
--         , 'Aeson.ToJSON' context
--         )
--     => 'Rfc7807Error' errorType errorInfo context
--     -> 'Aeson.Encoding'
-- @
toKeyValue
    :: forall kv errorType errorInfo context
    .   ( Aeson.ToJSON errorType
        , Aeson.ToJSON errorInfo
        , Aeson.ToJSON context
        , Aeson.KeyValue kv
        , Monoid kv
        )
    => EncodingOptions
    -> Rfc7807Error errorType errorInfo context
    -> kv
toKeyValue EncodingOptions{..} Rfc7807Error{..} = mconcat
    [ "type" .= type_
    , field "title" title
    , field "status" status
    , field "detail" detail
    , field "instance" instance_
    , extField ErrorField error_
    , extField ContextField context
    ]
  where
    field :: Aeson.ToJSON a => Text -> Maybe a -> kv
    field name value =
        mwhen (not omitNothingFields || isJust value)
            (name .= value)

    extField :: Aeson.ToJSON a => ExtensionField -> Maybe a -> kv
    extField name value =
        mwhen (not (omitExtensionField name))
            $ field (extensionFieldName name) value

    mwhen :: Bool -> kv -> kv
    mwhen p kv = if p then kv else mempty

-- }}} JSON Encoding ----------------------------------------------------------

-- {{{ JSON Decoding ----------------------------------------------------------

-- | Decode 'Rfc7807Error' using default 'EncodingOptions':
-- @
-- 'Aeson.parseJSON' = Aeson.withObject \"Rfc7807Error\" \\o ->
--     'parseJSON' 'defaultEncodingOptions' o
-- @
instance
    ( Aeson.FromJSON errorType
    , Aeson.FromJSON errorInfo
    , Aeson.FromJSON context
    , Typeable errorType
    , Typeable errorInfo
    , Typeable context
    )
    => Aeson.FromJSON (Rfc7807Error errorType errorInfo context)
  where
    parseJSON
        :: Aeson.Value
        -> Aeson.Parser (Rfc7807Error errorType errorInfo context)
    parseJSON = Aeson.withObject typeName (parseJSON defaultEncodingOptions)
      where
        typeName :: String
        typeName =
            show (typeRep (Proxy @(Rfc7807Error errorType errorInfo context)))

-- | Parse JSON value into 'Rfc7807Error'. Reason for taking 'Aeson.Object'
-- instead of 'Aeson.Value' is that it allows us to define serialisation for
-- our own data types with extra fields without while keeping RFC7807
-- structure.
parseJSON
    :: forall errorType errorInfo context
    .   ( Aeson.FromJSON errorType
        , Aeson.FromJSON errorInfo
        , Aeson.FromJSON context
        )
    => EncodingOptions
    -> Aeson.Object
    -> Aeson.Parser (Rfc7807Error errorType errorInfo context)
parseJSON EncodingOptions{omitExtensionField, extensionFieldName} o = do
    type_ <- o .: "type"
    title <- o .:? "title"
    status <- o .:? "status"
    detail <- o .:? "detail"
    instance_ <- o .:? "instance"
    error_ <- extField ErrorField
    context <- extField ContextField

    pure Rfc7807Error
        { type_
        , title
        , status
        , detail
        , instance_
        , error_
        , context
        }
  where
    extField
        :: Aeson.FromJSON a
        => ExtensionField
        -> Aeson.Parser (Maybe a)
    extField name
      | omitExtensionField name = pure Nothing
      | otherwise               = o .:? extensionFieldName name

-- }}} JSON Decoding ----------------------------------------------------------

-- $usageExamples
--
-- == Type Alias
--
-- The easiest way how to use 'Rfc7807Error' data type without always needing
-- to pass all the type arguments is by creating a type alias like this:
--
-- @
-- type ErrorResponse = 'Rfc7807Error' ErrorType () ()
--
-- data ErrorType
--     = DocumentNotFound
--     {- ... -}
--
-- instance 'Aeson.ToJSON' ErrorType where
--     toJSON = \\case
--         DocumentNotFound ->
--             'Aeson.Text' \"https://example.com/docs/error#document-not-found\"
--         {- ... -}
-- @
--
-- == Newtype
--
-- While it is possible to use 'Rfc7807Error' directly, using newtype allows to
-- be more flexible with how things are encoded. If you're expecting your use
-- cases to evolve over time it is good to start with something like this:
--
-- @
-- -- | See \"Type Alias\" section for \@ErrorType\@ example.
-- data ErrorType
--   = {- ... -}
--
-- newtype ErrorResponse = ErrorResponse
--     { errorResponse :: 'Rfc7807Error' ErrorType () ()
--     }
--
-- errorResponseEncodingOptions :: 'EncodingOptions'
-- errorResponseEncodingOptions = 'defaultEncodingOptions'
--     { 'omitExtensionField' = const True
--     }
--
-- instance 'Aeson.ToJSON' ErrorResponse where
--     'Aeson.toJSON' :: ErrorResponse -> 'Aeson.Value'
--     'Aeson.toJSON' ErrorResponse{..} =
--          'Aeson.object' . 'toKeyValue' errorResponseEncodingOptions
--     {- ... -}
--
-- instance 'Aeson.FromJSON' ErrorResponse where
--     'Aeson.parseJSON' :: ErrorResponse -> 'Aeson.Value'
--     'Aeson.parseJSON' = 'Aeson.withObject' \"ErrorResponse\" \\o ->
--          ErrorResponse <$> 'parseJSON' errorResponseEncodingOptions o
-- @
--
-- == Extra Fields Example
--
-- This is an elaboration of the previous \"Newtype\" example. We will use
-- @errorInfo@ and @context@ type arguments of 'Rfc7807Error' to include more
-- information. The @errorInfo@ will be kept polymorphic so that each HTTP
-- response can use a different one, depending on its needs.
--
-- @
-- -- | See \"Type Alias\" section for \@ErrorType\@ example.
-- data ErrorType
--   = {- ... -}
--
-- -- | We can use a concrete data type or we can use something flexible like
-- -- 'Aeson.Object' (actually a \@HashMap Text 'Aeson.Value'\@) allowing us to
-- -- include any kind of metadata.
-- --
-- -- This approach intentionally resembles structured logging approach like
-- -- the one used by [katip](https://hackage.haskell.org/package/katip) library.
-- type ErrorContext = 'Aeson.Object'
--
-- newtype ErrorResponse e = ErrorResponse
--     { errorResponse :: 'Rfc7807Error' ErrorType e ErrorContext
--     }
--
-- errorResponseEncodingOptions :: 'EncodingOptions'
-- errorResponseEncodingOptions = 'defaultEncodingOptions'
--
-- instance 'Aeson.ToJSON' => 'Aeson.ToJSON' (ErrorResponse e) where
--     'Aeson.toJSON' :: ErrorResponse -> 'Aeson.Value'
--     'Aeson.toJSON' ErrorResponse{..} =
--          'Aeson.object' . 'toKeyValue' errorResponseEncodingOptions
--     {- ... -}
--
-- instance 'Aeson.FromJSON' e => 'Aeson.FromJSON' (ErrorResponse e) where
--     'Aeson.parseJSON' :: ErrorResponse -> 'Aeson.Value'
--     'Aeson.parseJSON' = 'Aeson.withObject' \"ErrorResponse\" \\o ->
--          ErrorResponse <$> 'parseJSON' errorResponseEncodingOptions o
-- @
--
-- At this point we may want to provide few helper functions for constructing
-- @ErrorResponse@ (also known as smart constructors) to fit in nicely with the
-- rest of our code base and HTTP framework we are using. You may want to look
-- at "Servant.RFC7807" module, even if you're using a different framework. It
-- should give you few ideas on how to proceed.
