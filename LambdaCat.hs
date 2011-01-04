{-# LANGUAGE OverloadedStrings #-}

module LambdaCat
    ( lambdacat
    , defaultConfig
    , defaultModifySupplierURI
    , LambdaCatConf (..)
    )
where

import LambdaCat.Configure
import LambdaCat.Supplier
import LambdaCat.Supplier.Web
import LambdaCat.CmdArgs
import LambdaCat.View.Web (webView)
import LambdaCat.UI.Glade as UI
import LambdaCat.Utils

import Config.Dyre
import Config.Dyre.Compile
import Network.URI
import System.Exit
import System.IO

defaultConfig :: LambdaCatConf
defaultConfig = LambdaCatConf
    { modifySupplierURI = defaultModifySupplierURI
    , supplierList = [ (webSupplier   , ["http:","https:","about:"])
                     ]
    , viewList     = [ (webView, ["about:","http:", "https:", "file:"], [])
                     ]
    , homeURI      = "http://www.haskell.org"
    }

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

prepend :: String -> URI -> URI
prepend prfx uri = stringToURI $ prfx ++ show uri

mainCat :: (Maybe String, LambdaCatConf) -> IO ()
mainCat (e, cfg) = do
    maybe (return ()) error e

    setLCC cfg
    args <- getCmdArgs

    let  uria = map stringToURI $ uris args
         us   = if null uria
                then [homeURI cfg]
                else uria
    ui <- UI.init :: IO GladeUI
    mapM_ (supplyForView (UI.update ui undefined) embedView) us
    mainLoop ui

lambdacat :: LambdaCatConf -> IO ()
lambdacat cfg = do
    args <- getCmdArgs

    if recompile args
        then do
            customCompile dparams
            me <- getErrorString dparams
            case me of
                Just e  -> do
                    hPutStrLn stderr e
                    exitFailure
                Nothing -> return ()
        else wrapMain dparams (Nothing, cfg)

dparams :: Params (Maybe String, LambdaCatConf)
dparams =
    let dps = defaultParams
            { projectName = "lambdacat"
            , realMain    = mainCat
            , showError   = \ (_, c) s -> (Just s, c)
            , statusOut   = putStrLn
            }
    in  dps { ghcOpts = ["-eventlog", "-threaded"] }
