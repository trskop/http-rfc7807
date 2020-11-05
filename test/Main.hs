-- |
-- Module:      Main
-- Description: Unit tests for Haskell implementation of RFC7807 style responses
-- Copyright:   (c) 2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Unit tests for Haskell implementation of RFC7807 style responses.
module Main
    ( main
    )
  where

import Prelude

import Data.Maybe (isJust)
import Data.Proxy (Proxy(Proxy))

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Network.HTTP.Types (hContentType)
import Servant.API as Servant (JSON)
import Servant.Server as Servant
import Test.Hspec.Expectations.Json (shouldBeJson)
--import Test.QuickCheck.Instances ()
import Test.Tasty (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ({-(@?=),-} testCase, assertEqual, assertFailure)
--import Test.Tasty.QuickCheck (testProperty)

import Network.HTTP.RFC7807 (Rfc7807Error(..))
import Servant.Server.RFC7807 (ProblemJSON, rfc7807ServerError)


main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ testGroup "Network.HTTP.RFC7807"
        [ testDefaultSerialisation
        ]

    , testGroup "Servant.Server.RFC7807"
        [ testCase "err400, JSON" do
            let actual =
                    rfc7807ServerError @(Rfc7807Error Text () ())
                        (Proxy @Servant.JSON) Servant.err400 "/errors#400" id

            assertEqual "HTTP status code hasn't changed"
                (Servant.errHTTPCode Servant.err400)
                (Servant.errHTTPCode actual)
            assertEqual "HTTP reason phrase hasn't changed"
                (Servant.errReasonPhrase Servant.err400)
                (Servant.errReasonPhrase actual)
            assertEqual "Content-Type is present and is\
                \ application/json;charset=utf-8"
                (Just "application/json;charset=utf-8")
                (hContentType `lookup` Servant.errHeaders actual)

            case Aeson.eitherDecode (Servant.errBody actual) of
                Left err ->
                    assertFailure
                        ( "HTTP response body is valid JSON, but we got:\n"
                        <> err
                        )

                Right actualJson ->
                    actualJson `shouldBeJson` Aeson.object
                    [ "type" .= Aeson.String "/errors#400"
                    , "status" .= Servant.errHTTPCode Servant.err400
                    , "title" .= Servant.errReasonPhrase Servant.err400
                    ]

        , testCase "err500, ProblemJSON" do
            let actual =
                    rfc7807ServerError @(Rfc7807Error Text () ())
                        (Proxy @ProblemJSON) Servant.err500 "/errors#500" id

            assertEqual "HTTP status code hasn't changed"
                (Servant.errHTTPCode Servant.err500)
                (Servant.errHTTPCode actual)
            assertEqual "HTTP reason phrase hasn't changed"
                (Servant.errReasonPhrase Servant.err500)
                (Servant.errReasonPhrase actual)
            assertEqual "Content-Type is present and is\
                \ application/json;charset=utf8"
                (Just "application/problem+json;charset=utf-8")
                (hContentType `lookup` Servant.errHeaders actual)

            case Aeson.eitherDecode (Servant.errBody actual) of
                Left err ->
                    assertFailure
                        ( "HTTP response body is valid JSON, but we got:\n"
                        <> err
                        )

                Right actualJson ->
                    actualJson `shouldBeJson` Aeson.object
                        [ "type" .= Aeson.String "/errors#500"
                        , "status" .= Servant.errHTTPCode Servant.err500
                        , "title" .= Servant.errReasonPhrase Servant.err500
                        ]
        ]
    ]

testDefaultSerialisation :: TestTree
testDefaultSerialisation = testGroup "Default serialisation"
    -- Reason why samples are enough is that the way the serialisation works is
    -- mostly uniform. Testing it deeper just reimplements the logic and
    -- doesn't test anything useful.
    --
    -- Optional data values do not matter either as 'ToJSON' instances are
    -- used, therefore, we cannot guarantee anything beyond it being
    -- serialisable.
    --
    -- While testing that toJSON and toEncoding correspond would be useful to
    -- catch some errors, it won't test 'toKeyValue'.  Polymorphism guarantees
    -- that the representations cannot diverge beyond ordering.
    [ testDefaultSerialisationCase "Minimal sample" Rfc7807Error
        { type_ = "https://example.com/docs/error" :: Text
        , title = Nothing
        , status = Nothing
        , detail = Nothing
        , instance_ = Nothing
        , error_ = Nothing @()
        , context = Nothing @()
        }

    , testDefaultSerialisationCase "Full sample" Rfc7807Error
        { type_ = "https://example.com/docs/error" :: Text
        , title = Just "Not so detailed error"
        , status = Just 500
        , detail = Just "Very detailed error message"
        , instance_ = Just "https://example.com/error/instance/123"
        , error_ = Just $ Aeson.object
            [ "foo" .= Aeson.String "bar"
            ]
        , context = Just $ Aeson.object
            [ "id" .= Aeson.String "1234"
            ]
        }

    -- Missing values were strategically chosen to have one among fields
    -- defined by the standard and one among the extension fields.  As the
    -- logic is uniform we are able to verify both classes at the same time.
    , testDefaultSerialisationCase "Few empty values sample" Rfc7807Error
        { type_ = "https://example.com/docs/error" :: Text
        , title = Just "Not so detailed error"
        , status = Just 500
        , detail = Just "Very detailed error message"
        , instance_ = Nothing  -- Check that field is omitted.
        , error_ = Nothing @() -- Check that field is omitted.
        , context = Just ()
        }
    ]

testDefaultSerialisationCase
    ::  ( Aeson.ToJSON errorType
        , Aeson.ToJSON errorInfo
        , Aeson.ToJSON context
        )
    => TestName
    -> Rfc7807Error errorType errorInfo context
    -> TestTree
testDefaultSerialisationCase name v@Rfc7807Error{..} = testCase name do
    Aeson.toJSON v `shouldBeJson` Aeson.object
        ( mconcat
            [ ["type" .= type_]
            , ["title" .= title | isJust title]
            , ["status" .= status | isJust status]
            , ["detail" .= detail | isJust detail]
            , ["instance" .= instance_ | isJust instance_]
            , ["error" .= error_ | isJust error_]
            , ["context" .= context | isJust context]
            ]
        )
