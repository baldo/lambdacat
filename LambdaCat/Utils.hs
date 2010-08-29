{-# LANGUAGE TemplateHaskell #-}

module LambdaCat.Utils
    ( info
    , log

    , pinfo
    , plog

    , DShow (..)
    , dprint
    )
where

import Prelude hiding (log)
import Control.Monad
import Language.Haskell.TH
import Network.URI
import System.Console.CmdArgs.Verbosity

import Flags

unlessQuiet :: IO () -> IO ()
unlessQuiet f = do
    v <- getVerbosity
    when (v /= Quiet) f

$( if debug
       then [d| info :: (a -> IO ()) -> a -> IO ()
                info f = unlessQuiet . f

                log :: (a -> IO ()) -> a -> IO ()
                log f = unlessQuiet . f
            |]

       else [d| info :: (a -> IO ()) -> a -> IO ()
                info f = whenNormal . f

                log :: (a -> IO ()) -> a -> IO ()
                log f = whenLoud . f
            |]
 )

pinfo :: Q Exp
pinfo = do
    loc <- location
    let fn     = loc_filename loc
        (l, c) = loc_start loc
        pos    = fn ++ ":" ++ show l ++ ":" ++ show c ++ ": "
    if debug
        then [| \ f -> info (\ x -> putStr pos >> f x) |]
        else [| info |]

plog :: Q Exp
plog = do
    loc <- location
    let fn     = loc_filename loc
        (l, c) = loc_start loc
        pos    = fn ++ ":" ++ show l ++ ":" ++ show c ++ ": "
    if debug
        then [| \ f -> log (\ x -> putStr pos >> f x) |]
        else [| log |]

class DShow a where
    dshow :: a -> String
    dshow x = dshows x ""

    dshows :: a -> ShowS
    dshows x = const $ dshow x

instance DShow a => DShow (Maybe a) where
    dshows (Just x) = showString "Just " . dshows x
    dshows Nothing  = showString "Nothing"

instance DShow URI where
    dshows URI
        { uriScheme    = s
        , uriAuthority = a
        , uriPath      = p
        , uriQuery     = q
        , uriFragment  = f
        } = showString "URI: scheme = "
          . shows s
          . showString ", auth = ("
          . dshows a
          . showString "), path = "
          . shows p
          . showString ", query = "
          . shows q
          . showString ", fragment = "
          . shows f

instance DShow URIAuth where
    dshows URIAuth
        { uriUserInfo = u
        , uriRegName  = r
        , uriPort     = p
        } = showString "URIAuth: user = "
          . shows u
          . showString ", reg = "
          . shows r
          . showString ", port = "
          . shows p

dprint :: DShow a => a -> IO ()
dprint = putStrLn . dshow

