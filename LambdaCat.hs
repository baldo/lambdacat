{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : LambdaCat
-- Copyright   : Andreas Baldeau, Daniel Ehlers
-- License     : BSD3
-- Maintainer  : Andreas Baldeau <andreas@baldeau.net>,
--               Daniel Ehlers <danielehlers@mindeye.net>
-- Stability   : Alpha
--
-- This module (re-)exports the most important functions and datatypes you
-- need, to write your own configuration file.

module LambdaCat
    (
      -- * Main entry point
      lambdacat

      -- * Configuration
    , LambdaCatConf (..)

    , defaultConfig
    , defaultModifySupplierURI
    )
where

import System.Exit
import System.IO

import Config.Dyre
import Config.Dyre.Compile

import LambdaCat.CmdArgs
import LambdaCat.Configure
import LambdaCat.Supplier
import LambdaCat.Supplier.Web
import LambdaCat.UI.Glade as UI
import LambdaCat.Utils.URI
import LambdaCat.View.Web

-- | Lambdacat's default configuration.
defaultConfig :: LambdaCatConf
defaultConfig = LambdaCatConf
    { modifySupplierURI = defaultModifySupplierURI
    , supplierList      = [ SupplierSpec
                                (WebSupplierConf "")
                                [ "http:"
                                , "https:"
                                , "about:"
                                ]
                          ]
    , downloadHook      = [ SupplierSpec
                                (WebSupplierConf "/tmp")
                                [ "http:"
                                , "https:"
                                , "about:"
                                ]
                          ]
    , viewList          = [ ViewSpec
                                webViewConf
                                [ "about:"
                                , "http:"
                                , "https:"
                                , "file:"
                                ]
                                []
                          ]
    , homeURI           = "http://www.haskell.org"
    , defaultURI        = "about:blank"
    , defaultTitle      = "(Untitled)"
    , uiConfiguration   = UISpec gladeUIConf
    }

-- | The URI modifier used in the default configuration. It tries to add a
-- proper protocol if none is specified.
defaultModifySupplierURI :: URI -> URI
defaultModifySupplierURI uri@URI
    { uriScheme    = ""
    , uriAuthority = Just _
    } = prepend "http://" uri
defaultModifySupplierURI uri@URI
    { uriScheme    = ""
    , uriAuthority = Nothing
    , uriPath      = '/' : _
    } = prepend "file://" uri
defaultModifySupplierURI uri@URI
    { uriScheme    = ""
    , uriAuthority = Nothing
    , uriPath      = _ : _
    } = prepend "http://" uri
defaultModifySupplierURI uri = uri

-- | Add a String to the beginning of the given URI.
prepend :: String -> URI -> URI
prepend prfx uri = stringToURI $ prfx ++ show uri

-- | This is the real main function. It is called by the dyre stack.
mainCat
    :: Maybe String   -- ^ Just the error that occured during the compilation
                      -- of the user configuration, Nothing if none occured.
    -> LambdaCatConf  -- ^ The users configuration.
    -> CmdArgs        -- ^ The commandline arguments.
    -> IO ()
mainCat e cfg args = do
    maybe (return ()) error e

    setLCC cfg

    let uria = uris args
        us   = if null uria
                   then [homeURI cfg]
                   else uria
    case (uiConfiguration lambdaCatConf) of
        UISpec uiconf -> do
            ui <- UI.init uiconf
            mapM_ (supplyForView (UI.update ui undefined) embedView) us
            mainLoop ui

-- | Lambdacat's main function. It processes commandline parameters, handles
-- recompilation of the user configuration and calls the real main function.
-- Use this as the main function in your user configuration file.
lambdacat
    :: LambdaCatConf  -- ^ Configuration to use. Just start with
                      -- 'defaultConfig' and overwrite fields as you wish.
    -> IO ()
lambdacat cfg = do
    args <- getCmdArgs

    if recompile args
        then do
            customCompile dparams
            me <- getErrorString dparams

            case me of
                Just e -> do
                    hPutStrLn stderr e
                    exitFailure

                Nothing ->
                    return ()

        else wrapMain dparams (Nothing, cfg, args)

-- | Like 'uncurry', only for 3-tuples.
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

-- | Configuration for dyre.
dparams :: Params (Maybe String, LambdaCatConf, CmdArgs)
dparams =
    let dps = defaultParams
            { projectName = "lambdacat"
            , realMain    = uncurry3 mainCat
            , showError   = \(_, c, a) s -> (Just s, c, a)
            , statusOut   = putStrLn
            }
    in  dps { ghcOpts = ["-eventlog"] }

