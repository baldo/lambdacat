{-# LANGUAGE CPP #-}

module LambdaCat
    ( lambdacat
    , defaultConfig
    )
where

import LambdaCat.Browser
import LambdaCat.Configure
import LambdaCat.CmdArgs
import LambdaCat.Page.Cat
import LambdaCat.Page.MPlayer
import LambdaCat.Page.Poppler
import LambdaCat.Page.WebView
import LambdaCat.UI.Glade
import qualified LambdaCat.Page as Page
import qualified LambdaCat.Page as UI

import Data.Maybe
import Config.Dyre
import Config.Dyre.Compile
import Control.Monad
import Graphics.UI.Gtk.WebKit.WebView
import Network.URI
import System
import System.IO

defaultConfig :: LambdaCatConf
defaultConfig = LambdaCatConf 
    { pageList = [ (webViewPage , ["http:","https:"])
                 , (popplerPage , ["file:"])
                 , (mplayerPage , ["mms:"])
                 , (catPage     , ["cat:"])
                 ]
    }

mainCat :: (Maybe String, LambdaCatConf) -> IO ()
mainCat (e, cfg) = do
    maybe (return ()) error e

    setLCC cfg
    args <- getCmdArgs

    let us = if null $ uris args
             then ["http://www.haskell.org"]
             else uris args
    ui <- UI.init :: IO GladeUI
    browser <- UI.newBrowser ui :: IO BrowserId
    mapM_ (\ uri -> do
        mpage <- Page.pageFromProtocol (UI.update ui browser) (pageList lambdaCatConf) Nothing (parseURI uri)
        case mpage of
            (Just page) -> do
                UI.embedPage ui browser page
                Page.load page (fromJust $ parseURI uri)
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
dparams = defaultParams 
    { projectName = "lambdacat"
    , realMain    = mainCat
    , showError   = \ (_, c) s -> (Just s, c)
#ifdef DEBUG
    , ghcOpts     = ["-eventlog", "-threaded"]
#endif
    }
