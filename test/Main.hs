-- |
-- Module:      Main
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module Main
    ( main
    )
  where

import Prelude

import Test.Tasty (TestTree, defaultMain, testGroup)
--import Test.Tasty.HUnit ((@?=), testCase, assertEqual)


main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ testGroup "Network.HTTP.RFC7807"
        [
        ]
    ]
