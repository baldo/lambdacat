{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module LambdaCat.View.Web
    ( WebView
    , webView

    , module LambdaCat.View
    ) where

import LambdaCat.Configure
import LambdaCat.View
import LambdaCat.UI 
import LambdaCat.Utils
import LambdaCat.Supplier

import Data.Maybe
import qualified Graphics.UI.Gtk.WebKit.WebView as WV
import Graphics.UI.Gtk.Abstract.Widget
-- import Graphics.UI.Gtk.WebKit.WebSettings
-- import Graphics.UI.Gtk.WebKit.WebFrame
-- import Graphics.UI.Gtk.WebKit.Download
import qualified Graphics.UI.Gtk.WebKit.NetworkRequest as NR
import qualified Graphics.UI.Gtk.WebKit.WebNavigationAction as NA
import Graphics.UI.Gtk hiding (populatePopup,widgetDestroy)
import Network.URI
-- import System.Directory
-- import System.FilePath
-- import System.Glib.GError

-- | Data type representing the view. Do not confuse this with WebKit's WebView.
newtype WebView = WebView { webViewWidget :: WV.WebView }

-- | Configuration constant
webView :: View
webView = View (WebView undefined)

instance ViewClass WebView where
    new = do
        widget <- WV.webViewNew
        return WebView { webViewWidget = widget }

    embed wV@(WebView { webViewWidget = widget }) embedder callback = do
        -- Setup signal handling
        _ <- widget `on` WV.navigationPolicyDecisionRequested $ \ _wf nr na _wpd -> do            
          muri <- NR.networkRequestGetUri nr
          reason <- NA.webNavigationActionGetReason na
          case muri of 
            Just uri -> 
              case reason of 
                NA.WebNavigationReasonFormResubmitted -> return False -- this is not handled because of the form data
                NA.WebNavigationReasonLinkClicked -> do 
                  supplyForView callback replaceView $ stringToURI uri
                  return True
                _ -> return False
            Nothing  -> return False

        _ <- widget `on` WV.newWindowPolicyDecisionRequested $ \ _wf nr _na _wpd -> do
          muri <- NR.networkRequestGetUri nr
          case muri of 
            Just uri -> supplyForView callback replaceView $ stringToURI uri
            Nothing  -> return ()  
          return True

        _ <- widget `on` WV.titleChanged  $ \ _wf _title -> callback (changedTitle $ View wV)
        _ <- widget `on` WV.loadStarted   $ \ _wf -> callback (changedURI $ View wV)
        _ <- widget `on` WV.loadCommitted $ \ _wf -> callback (changedURI $ View wV)
        _ <- widget `on` WV.loadFinished  $ \ _wf -> callback (changedURI $ View wV)

        _ <- widget `on` WV.progressChanged  $ \ progress -> callback (changedProgress progress)

        -- Embed widget 
        embedder $ castToWidget widget

    destroy WebView { webViewWidget = widget } = do 
        -- TODO: Unref WebKit's WebView.
        --WV.webViewLoadUri widget "about:blank"
        WV.webViewStopLoading widget
        widgetDestroy widget 

    load WebView { webViewWidget = widget } uri = do
        -- TODO: Write module for URI conversion
        WV.webViewLoadUri widget (uriToString id uri "")
        return True

    getCurrentURI WebView { webViewWidget = widget } = do
        mUriStr <- WV.webViewGetUri widget
        return $ maybe nullURI stringToURI mUriStr

    getCurrentTitle WebView { webViewWidget = widget } = do
        mTitle <- WV.webViewGetTitle widget
        return $ fromMaybe (defaultTitle lambdaCatConf) mTitle

    getCurrentProgress WebView { webViewWidget = widget } = do
        progress <- widget `get` WV.webViewProgress 
        status   <- widget `get` WV.webViewLoadStatus 
        case status of 
          WV.LoadFinished -> return 100
          WV.LoadFailed   -> return 100
          _               -> return $ round (progress * 100)
