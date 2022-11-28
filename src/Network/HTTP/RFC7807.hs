-- |
-- Module:      Network.HTTP.RFC7807
-- Description: RFC7807 style response messages
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
    -- $intro
      Rfc7807Error(..)
    , rfc7807Error

    -- * Encoding and Decoding
    --
    -- | #encoding-and-decoding# Definitions in this section are useful for
    -- defining your own JSON encoding\/decoding. See [Usage Examples section
    -- ](#usage-examples) for ideas on how to use them.
    --
    -- What's provided in here are:
    --
    -- * Function 'toKeyValue' for generic serialisation of 'Rfc7807Error' into
    --   JSON object representation.
    --
    -- * Function 'parseObject' for parsing JSON 'Aeson.Object' (key-value map)
    --   into 'Rfc7807Error'.
    --
    -- * Parameters that modify behaviour of 'toKeyValue' and 'parseObject:
    --   'EncodingOptions', 'defaultEncodingOptions', and 'ExtensionField'.
    , toKeyValue
    , parseObject
    , EncodingOptions(..)
    , defaultEncodingOptions
    , ExtensionField(..)

    -- * Usage Examples
    --
    -- $usageExamples

    -- ** Type Alias
    --
    -- $usageExamplesTypeAlias

    -- ** Newtype
    --
    -- $usageExamplesNewtype

    -- ** Extra Fields Example
    --
    -- $usageExamplesExtraFieldsExample
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
-- additional fields @error_ :: errorInfo@ and @context :: context@.
--
-- Meaning of individual type parameters:
--
-- [@errorType@]: Represents an URI reference. Easiest to start with is just
--   using 'Text' type; simplest and most extensible is defining an enum with a
--   'Aeson.ToJSON', see [Usage Examples section](#usage-examples) for an enum
--   example.
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
    -- ^ (__required__) A URI reference
    -- (see [RFC3986](https://tools.ietf.org/html/rfc3986)) that identifies the
    -- problem type.  This specification encourages that, when dereferenced, it
    -- provide human-readable documentation for the problem type (e.g., using
    -- HTML [W3C.REC-html5-20141028
    -- ](https://tools.ietf.org/html/rfc7807#ref-W3C.REC-html5-20141028)).
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
    -- relative to the document's base URI, as per [RFC3986, Section 5
    -- ](https://tools.ietf.org/html/rfc3986#section-5).
    --
    -- === Notes:
    --
    -- In JSON this filed is named only @\"type\"@.

    , title :: Maybe Text
    -- ^ (__optional__) A short, human-readable summary of the problem type. It
    -- SHOULD NOT change from occurrence to occurrence of the problem, except
    -- for purposes of localization (e.g., using proactive content negotiation;
    -- see [RFC7231, Section 3.4
    -- ](https://tools.ietf.org/html/rfc7231#section-3.4).
    --
    -- Consumers MUST use the @\"type\"@ string as the primary identifier for
    -- the problem type; the @\"title\"@ string is advisory and included only
    -- for users who are not aware of the semantics of the URI and do not
    -- have the ability to discover them (e.g., offline log analysis).
    -- Consumers SHOULD NOT automatically dereference the type URI.
    --
    -- === Notes:
    --
    -- In JSON this filed is named @\"title\"@.

    , status :: Maybe Int
    -- ^ (__optional__) The HTTP status code (see [RFC7231, Section 6
    -- ](https://tools.ietf.org/html/rfc7231#section-6)) generated by the
    -- origin server for this occurrence of the problem.
    --
    -- If present, is only advisory; it conveys the HTTP status code used for
    -- the convenience of the consumer.  Generators MUST use the same status
    -- code in the actual HTTP response, to assure that generic HTTP software
    -- that does not understand this format still behaves correctly.  See
    -- [RFC7807, Section 5](https://tools.ietf.org/html/rfc7807#section-5) for
    -- further caveats regarding its use.
    --
    -- Consumers can use the status member to determine what the original
    -- status code used by the generator was, in cases where it has been
    -- changed (e.g., by an intermediary or cache), and when message bodies
    -- persist without HTTP information.  Generic HTTP software will still use
    -- the HTTP status code.
    --
    -- === Notes:
    --
    -- In JSON this filed is named @\"status\"@.

    , detail :: Maybe Text
    -- ^ (__optional__) A human-readable explanation specific to this
    -- occurrence of the problem.
    --
    -- If present, ought to focus on helping the client correct the problem,
    -- rather than giving debugging information.  Consumers SHOULD NOT parse
    -- the "detail" member for information; extensions are more suitable and
    -- less error-prone ways to obtain such information.
    --
    -- === Notes:
    --
    -- In JSON this filed is named @\"detail\"@.

    , instance_ :: Maybe Text
    -- ^ (__optional__) A URI reference that identifies the specific occurrence
    -- of the problem.  It may or may not yield further information if
    -- dereferenced.
    --
    -- Relative URIs are accepted; this means that they must be resolved
    -- relative to the document's base URI, as per [RFC3986, Section 5
    -- ](https://tools.ietf.org/html/rfc3986#section-5).
    --
    -- === Notes:
    --
    -- In JSON this filed is named only @\"instance\"@.

    , error_ :: Maybe errorInfo
    -- ^ (__optional__, __extension__) An additional representation of the
    -- error.  Lots of clients detect that the response is an error using
    -- simple algorithm of checking presence of the field @\"error\"@ that has
    -- non-@null@ value.
    --
    -- === Notes:
    --
    -- How the field is named in the resulting JSON object is controlled by
    -- @extensionFieldName@ of 'EncodingOptions' and the default provided by
    -- 'defaultEncodingOptions' is @\"error\"@.

    , context :: Maybe context
    -- ^ (__optional__, __extension__) Extra information for the purposes of
    -- debugging.
    --
    -- === Notes:
    --
    -- How the field is named in the resulting JSON object is controlled by
    -- @extensionFieldName@ of 'EncodingOptions' and the default provided by
    -- 'defaultEncodingOptions' is @\"context\"@.
    }
  deriving stock (Eq, Generic, Show)

-- | Constructor for 'Rfc7807Error' that set's only @type_@ field value and
-- everything else is set to 'Nothing'.
--
-- === Usage Example
--
-- This example illustrates how the function is used, not necessarily the best
-- error response you can provide to your client:
--
-- @
-- ('rfc7807Error' \"/errors#not-found\"){status = 404}
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

-- | Enum representing the extension fields @error_@ and @context@ of
-- 'Rfc7807Error' that are not defined by RFC7807.
--
-- This allows us to reference the field in 'EncodingOptions' and later in
-- 'toKeyValue' and 'parseObject' without resolving to using 'Text'.
data ExtensionField
    = ErrorField
    -- ^ Represents the name of the @error_@ field of 'Rfc7807Error' data type.
    | ContextField
    -- ^ Represents the name of the @context@ field of 'Rfc7807Error' data type.
  deriving stock (Eq, Generic, Show)

-- {{{ JSON Encoding ----------------------------------------------------------

-- | Encode using @'toKeyValue' 'defaultEncodingOptions'@.
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
    -- ^ Should empty fields be omitted in the JSON representation?
    --
    -- [If set to @True@ (default)]: then record fields of 'Rfc7807Error' with
    -- a 'Nothing' value will be omitted from the resulting object instead of
    -- being represented as @null@.
    --
    -- [If set to @False@]: then the resulting JSON object will include those
    -- fields and the 'Nothing' value will be mapped to @null@ JSON value.
    --
    -- === Notes:
    --
    -- This setting is ignored by 'parseObject' function as respecting it would
    -- mean that even valid RFC7807 messages would fail to parse.

    , omitExtensionField :: ExtensionField -> Bool
    -- ^ Should specified extension field be omitted in the JSON
    -- representation?
    --
    -- [If the function returns @True@]: then the specified record field of
    -- 'Rfc7807Error' will be omitted entirely even if it contains
    -- 'Data.Maybe.Just' value.
    --
    -- [If the function returns @False@]: then the specified record field is
    -- included in the serialised output. However, if the value of that field
    -- is 'Nothing' and @omitNothingFields@ is set to @True@ then the field
    -- will once again be omitted from the resulting JSON object.
    --
    -- === Notes:
    --
    -- This setting can be used in a similar fashion as verbosity level. For
    -- example, we can omit these fields on production and have them enabled
    -- in testing or dev environments.
    --
    -- This setting is respected by 'parseObject' function, which will ignore
    -- extension fields for which the function returns @True@. Ignored
    -- extension fields will always be set to 'Nothing'.

    , extensionFieldName :: ExtensionField -> Aeson.Key
    -- ^ How should the extension fields be named?
    --
    -- Fields @error_@ and @context@ of 'Rfc7807Error' are not defined by
    -- RFC7807 and as such their names may be adjusted depending on our
    -- particular needs and conventions. This function allows exactly that.
    --
    -- === Notes:
    --
    -- This setting is respected by 'parseObject' function, which will use this
    -- function when searching for extension fields in a JSON object.
    }
  deriving stock (Generic)

-- | Default 'EncodingOptions':
--
-- @
-- defaultEncodingOptions = 'EncodingOptions'
--     { omitNothingFields = True
--     , omitExtensionField = const False
--     , extensionFieldName = \\case
--         'ErrorField'   -> \"error\"
--         'ContextField' -> \"context\"
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

-- | Serialise 'Rfc7807Error' into a key-value pairs. It's abstract to support
-- both types of Aeson encodings ('Aeson.Object' and 'Aeson.Encoding') at once.
--
-- === Usage Examples
--
-- @
-- 'Aeson.Object' . 'toKeyValue' 'defaultEncodingOptions'
--     ::  ( 'Aeson.ToJSON' errorType
--         , 'Aeson.ToJSON' errorInfo
--         , 'Aeson.ToJSON' context
--         )
--     => 'Rfc7807Error' errorType errorInfo context
--     -> 'Aeson.Value'
-- @
--
-- @
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
    field :: Aeson.ToJSON a => Aeson.Key -> Maybe a -> kv
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

-- | Decode using @'parseObject' 'defaultEncodingOptions'@.
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
    parseJSON = Aeson.withObject typeName (parseObject defaultEncodingOptions)
      where
        typeName :: String
        typeName =
            show (typeRep (Proxy @(Rfc7807Error errorType errorInfo context)))

-- | Parse JSON value into 'Rfc7807Error'. Reason for taking 'Aeson.Object'
-- instead of 'Aeson.Value' is that it allows us to define serialisation for
-- our own data types with extra fields while preserving RFC7807 message
-- structure.
--
-- === Usage example
--
-- @
-- 'Aeson.withObject' \"ErrorResponse\" \\o ->
--     'parseObject' 'defaultEncodingOptions' o
-- @
parseObject
    :: forall errorType errorInfo context
    .   ( Aeson.FromJSON errorType
        , Aeson.FromJSON errorInfo
        , Aeson.FromJSON context
        )
    => EncodingOptions
    -> Aeson.Object
    -> Aeson.Parser (Rfc7807Error errorType errorInfo context)
parseObject EncodingOptions{omitExtensionField, extensionFieldName} o = do
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

-- $intro
--
-- This module defines 'Rfc7807Error' data type that represents
-- [RFC7807](https://tools.ietf.org/html/rfc7807) style response message along
-- with few extensions that are not defined by the standard, but allowed by it.
--
-- The sandard specifies two serialisation formats:
--
-- 1. JSON (@application\/problem+json@) and
--
-- 2. XML (@application\/problem+xml@)
--
-- This package supports only JSON serialisation, but it should not be hard to
-- build XML serialisation yourself, if required. We also expose few low-level
-- definitions for cases when you want to build your own JSON serialisation
-- that is compatible with the standard. If you're interested in that then best
-- to look at [Usage Examples](#usage-examples) and [Encoding and Decoding
-- ](#encoding-and-decoding) sections.
--
-- This package also provides Servant integration that is defined in a separate
-- module "Servant.Server.RFC7807".
--
-- If you want to jump straight to using this then go directly to
-- [Usage Examples section](#usage-examples).

-- $usageExamples
--
-- #usage-examples#
--
-- We start with a simple use case in [Type Alias section
-- ](#usage-examples-type-alias) and we get progressively more complicated.
-- Which one is best for you depends on many factors. There's a little guidance
-- that we can give you in that regard, but maybe consider following:
--
-- * If you are just exploring or evaluating multiple options then maybe start
--   with the simple example first.
--
-- * If you want to integrate RFC7807 style messages into existing system,
--   while requiring backward compatibility, then go with the more complicated
--   example. It will allow you to merge existing error responses with RFC7807
--   style ones more easily.
--
-- Haskell\/GHC language extensions being used in the examples:
--
-- * @RecordWildCards@ and @NamedFieldPuns@ — please read this great article
--   if you're not familiar with these extensions: [The Power of RecordWildCards
--   by Dmitrii Kovanikov](https://kodimensional.dev/recordwildcards).
--
-- * @LambdaCase@ — allows us to use @\\case@ as a short hand for
--   @\\x -> case x of@. See [GHC User's Guide — Lambda-case
--   ](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html#lambda-case)
--   for more information.
--
-- * @OverloadedStrings@ — allows us to define string literals for types like
--   'Text' without needing to manually pack\/convert 'String' values. See
--   [GHC User's Guide — Overloaded string literals
--   ](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html#overloaded-string-literals)
--   for more information.

-- $usageExamplesTypeAlias
--
-- #usage-examples-type-alias#
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
--             'Aeson.String' \"https:\/\/example.com\/docs\/error#document-not-found\"
--         {- ... -}
-- @
--
-- If you want custom value in @\"error\"@ field then you can either specify
-- the type to the one you're using or leave @errorInfo@ type variable
-- polymorphic. The later has the advantage that different types can be used
-- for different REST API resources\/endpoints:
--
-- @
-- type ErrorResponse errorInfo = 'Rfc7807Error' ErrorType errorInfo ()
--
-- data ErrorType
--     = DocumentNotFound
--     {- ... -}
--
-- instance 'Aeson.ToJSON' ErrorType where
--     toJSON = \\case
--         DocumentNotFound ->
--             -- The URL doesn't have to be absolute. See description of
--             -- @type_@ field of 'Rfc7807Error' for more information.
--             'Aeson.String' \"https:\/\/example.com\/docs\/error#document-not-found\"
--         {- ... -}
-- @

-- $usageExamplesNewtype
--
-- While it is possible to use 'Rfc7807Error' directly, using newtype allows to
-- be more flexible with how things are encoded. If you're expecting your use
-- cases to evolve over time it is good to start with something like this:
--
-- @
-- -- | See [\"Type Alias\"](#usage-examples-type-alias) section for \@ErrorType\@ example.
-- data ErrorType
--   = {- ... -}
--
-- newtype ErrorResponse = ErrorResponse
--     { errorResponse :: 'Rfc7807Error' ErrorType () ()
--     }
--
-- -- Following encoding example is very simple, basicaly the same thing as the
-- -- default 'Rfc7807Error' encoding. However, it's a template that when
-- -- copied allows us to adjust bits that we want different.
--
-- errorResponseEncodingOptions :: 'EncodingOptions'
-- errorResponseEncodingOptions = 'defaultEncodingOptions'
--     { omitExtensionField = const True
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
--          ErrorResponse <$> 'parseObject' errorResponseEncodingOptions o
-- @

-- $usageExamplesExtraFieldsExample
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
-- -- Following serialisation example is just one of many possibilities. What
-- -- it illustrates is how much flexibility we have. Not only we can rename
-- -- fields through @extensionFieldName@ of 'EncodingOptions', we can also
-- -- play with the encoding -- to get something that is more suitable for our
-- -- system.
--
-- -- | What we'll do is serialise the \@ErrorContext\@ manually. To be able to
-- -- do that we need to tell 'toKeyValue' and 'parseObject' to ignore the
-- -- extension field.
-- --
-- -- Another thing that we'll do is that we'll rename the @\"error\"@ field to
-- -- @\"error_message\"@. This is one of those things that are useful when
-- -- we are changing existing error responses.
-- errorResponseEncodingOptions :: 'EncodingOptions'
-- errorResponseEncodingOptions = 'defaultEncodingOptions'
--     { omitExtensionField = \\case
--         'ErrorField' -> False
--         'ContextField' -> True
--
--     , extensionFieldName = \\case
--         'ErrorField' -> \"error_message\"
--         name -> extensionFieldName 'defaultEncodingOptions' name
--     }
--
-- instance 'Aeson.ToJSON' => 'Aeson.ToJSON' (ErrorResponse e) where
--     'Aeson.toJSON' :: ErrorResponse -> 'Aeson.Value'
--     'Aeson.toJSON' ErrorResponse{errorResponse} = 'Aeson.Object'
--         ( 'toKeyValue' errorResponseEncodingOptions errorResponse
--         -- We'll take everything that's in context and put it directly into
--         -- the top-level JSON object.
--         --
--         -- The downside of this approach is that we need to be careful not
--         -- to redefine already existing fields. What we could do is change
--         -- the field names. It is quite common to use \"@fieldName\" or
--         -- similar convention for metadata.
--         --
--         -- If we go with custom data type we can then examine if it's JSON
--         -- object or not. If not we can instead put it into the \"context\"
--         -- field as a kind of a default.
--         <> context errorResponse
--         )
--     {- ... -}
--
-- instance 'Aeson.FromJSON' e => 'Aeson.FromJSON' (ErrorResponse e) where
--     'Aeson.parseJSON' :: ErrorResponse -> 'Aeson.Value'
--     'Aeson.parseJSON' = 'Aeson.withObject' \"ErrorResponse\" \\o ->
--          errorResponse <- 'parseObject' errorResponseEncodingOptions o
--
--          -- Now we'll take all the fields that are not part of RFC7807 or
--          -- \"error\" and put them into context.
--          let context = flip filterWithKey o \\k _v ->
--                  k `notElem` parsedFields
--
--          pure ErrorResponse
--              { errorResponse = errorResponse{context}
--              }
--        where
--          parsedFields =
--              -- These hardcoded values are okay since RFC7807 defines the
--              -- names and we cannot change them.
--              [ \"type\", \"title\", \"status\", \"detail\", \"instance\"
--              , extensionFieldName 'ErrorField'
--              ]
-- @
--
-- At this point we may want to provide few helper functions for constructing
-- @ErrorResponse@ (also known as smart constructors) to fit in nicely with the
-- rest of our code base and HTTP framework we are using. You may want to look
-- at "Servant.Server.RFC7807" module, even if you're using a different
-- framework. It should give you few ideas on how to proceed.
