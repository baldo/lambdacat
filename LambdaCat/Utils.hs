{-# OPTIONS_GHC -fno-warn-orphans #-}

module LambdaCat.Utils
    ( stringToURI
    , showURI 
    )
where

import GHC.Exts (IsString (..))
import Network.URI

instance IsString URI where
    fromString = stringToURI

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

showAuth :: URIAuth -> String
showAuth URIAuth
    { uriUserInfo = userInfo
    , uriRegName  = regName
    , uriPort     = port
    } = "(userInfo = " ++ userInfo
     ++ ", regName = " ++ regName
     ++ ", port = " ++ port
     ++ ")"

