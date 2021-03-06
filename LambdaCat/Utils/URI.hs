{-# OPTIONS_GHC -fno-warn-orphans
                -fno-warn-unused-binds
  #-}

-- |
-- Module      : LambdaCat.Utils.URI
-- Copyright   : Andreas Baldeau, Daniel Ehlers
-- License     : BSD3
-- Maintainer  : Andreas Baldeau <andreas@baldeau.net>,
--               Daniel Ehlers <danielehlers@mindeye.net>
-- Stability   : Alpha
--
-- This module provides a bunch of functions and instances that are pretty
-- usefull when working with 'URI's.

module LambdaCat.Utils.URI
    (
      -- * The URI type
      URI (..)
    , nullURI

      -- * Conversion
    , IsString
    , stringToURI
    , failingStringToURI
    , showURI
    )
where

import GHC.Exts
    ( IsString (..)
    )
import Network.URI

instance IsString URI where
    fromString = stringToURI

-- | This function tries hard on parsing a given String and converting it to
-- an URI. In case of failure it returns 'nullURI'.
--
-- Use this function for parsing URIs where ever possible.
stringToURI :: String -> URI
stringToURI =
    tryParsers
        [ parseURI
        , parseURIReference
        , parseRelativeReference
        , parseAbsoluteURI
        ]
        nullURI
  where
    tryParsers []       e _   = e
    tryParsers (p : ps) e str =
        case p str of
            Just r  -> r
            Nothing -> tryParsers ps e str

-- | Like 'stringToURI', but fails if parsing results in a 'nullURI' instead
-- of return it.
failingStringToURI :: String -> URI
failingStringToURI uriString =
    if uri /= nullURI
        then uri
        else error $ "Invalid URI: " ++ uriString
  where
    uri = stringToURI uriString

-- | Convert a given URI into a String representation.
--
-- This function is espacially usefull for debugging since it exposes the real
-- structure of the URI datatype. The 'Show' instance for URI only pretty
-- prints the URI.
showURI :: URI -> String
showURI URI
    { uriScheme    = uScheme
    , uriAuthority = mAuth
    , uriPath      = uPath
    , uriQuery     = uQuery
    , uriFragment  = uFragment
    } = "URI: scheme = " ++ uScheme
     ++ ", auth = " ++ maybe "none" showAuth mAuth
     ++ ", path = " ++ uPath
     ++ ", query = " ++ uQuery
     ++ ", fragment = " ++ uFragment

-- | Helper function to show the auth part of an URI.
showAuth :: URIAuth -> String
showAuth URIAuth
    { uriUserInfo = userInfo
    , uriRegName  = regName
    , uriPort     = port
    } = "(userInfo = " ++ userInfo
     ++ ", regName = " ++ regName
     ++ ", port = " ++ port
     ++ ")"

