{-# LANGUAGE TemplateHaskell #-}

module LambdaCat
    ( lambdacat
    , defaultConfig
    , LambdaCatConf (..)
    )
where

import Flags
import LambdaCat.Configure
import LambdaCat.Supply
import LambdaCat.CmdArgs
import LambdaCat.View.Web
import LambdaCat.UI.Glade
import LambdaCat.Class 

import Prelude hiding (log)
import Config.Dyre
import Config.Dyre.Compile
import Data.Maybe
import Network.URI
import System.Exit
import System.IO

defaultConfig :: LambdaCatConf
defaultConfig = LambdaCatConf
    { supplierList = [ (webSupplier   , ["http:","https:"])
                     , (aboutSupplier , ["about:"])
                     ]
    , pageList     = [ (webViewPage, ["http:", "https:", "file:"], [])
                     ]
    , homeURI      = fromJust $ parseURI "http://www.haskell.org"
    }

mainCat :: (Maybe String, LambdaCatConf) -> IO ()
mainCat (e, cfg) = do
    maybe (return ()) error e

    setLCC cfg
    args <- getCmdArgs

    let us = if null $ uris args
             then [show $ homeURI cfg]
             else uris args
    ui <- UI.init :: IO GladeUI
    browser <- UI.newBrowser ui :: IO BrowserId
    mapM_ (\ uri -> do
        mpage <- View.pageFromProtocol (UI.update ui browser) (uriModifier lambdaCatConf) (pageList lambdaCatConf) Nothing (parseURIReference uri)
        case mpage of
            (Just (page, uri')) -> do
                UI.embedPage ui browser page
                _ <- View.load page uri'
                return ()
            Nothing     -> return ()
        ) us
    UI.mainLoop ui

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
            , statusOut   = $plog putStrLn
            }
    in  if debug
            then dps { ghcOpts = ["-eventlog", "-threaded"] }
            else dps
