{-# LANGUAGE CPP #-}

module LambdaCat
    ( lambdacat
    , defaultConfig
    )
where

import LambdaCat.Browser
import LambdaCat.Configure
import LambdaCat.Page.Cat
import LambdaCat.Page.MPlayer
import LambdaCat.Page.Poppler
import LambdaCat.Page.WebView
import LambdaCat.UI.Glade
import qualified LambdaCat.Page as Page
import qualified LambdaCat.Page as UI

import Data.Maybe
import qualified Config.Dyre as Dyre
import Graphics.UI.Gtk.WebKit.WebView
import Network.URI
import System

defaultConfig :: LambdaCatConf
defaultConfig = LambdaCatConf 
    { pageList = [ (webViewPage , ["http:","https:"])
                 , (popplerPage , ["file:"])
                 , (mplayerPage , ["mms:"])
                 , (catPage     , ["cat:"])
                 ]
    }

mainCat :: LambdaCatConf -> IO ()
mainCat cfg = do
    setLCC cfg
    args <- getArgs
    let uris = if null args
               then ["http://www.haskell.org"]
               else args
    ui <- UI.init :: IO GladeUI
    browser <- UI.newBrowser ui :: IO BrowserID
    mapM_ (\ uri -> do
        mpage <- Page.pageFromProtocol (UI.update ui browser) (pageList lambdaCatConf) Nothing (parseURI uri)
        case mpage of
            (Just page) -> do
                UI.embedPage ui browser page
                Page.load page (fromJust $ parseURI uri)
                return ()
            Nothing     -> return ()
        ) uris
    UI.mainLoop ui

lambdacat = Dyre.wrapMain $ Dyre.defaultParams 
    { Dyre.projectName = "lambdacat"
    , Dyre.realMain    = mainCat
    , Dyre.showError   = \ c s -> c
#ifdef DEBUG
    , Dyre.ghcOpts     = ["-eventlog", "-threaded"]
#endif
    }
