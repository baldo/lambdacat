{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , TypeFamilies
  #-}

-- |
-- Module      : LambdaCat.View.Web
-- Copyright   : Andreas Baldeau, Daniel Ehlers
-- License     : BSD3
-- Maintainer  : Andreas Baldeau <andreas@baldeau.net>,
--               Daniel Ehlers <danielehlers@mindeye.net>
-- Stability   : Alpha
--
-- This module provides the 'WebView'. Do confuse this with WebKit's WebView!

module LambdaCat.View.Web
    (
      -- * The View
      WebView
    , webViewConf
    )
where

import Data.Maybe
-- import System.Directory
-- import System.FilePath

import Graphics.UI.Gtk hiding
    ( populatePopup
    , widgetDestroy
    )
import Graphics.UI.Gtk.Abstract.Widget
-- import Graphics.UI.Gtk.WebKit.Download
import qualified Graphics.UI.Gtk.WebKit.NetworkRequest as NR
-- import Graphics.UI.Gtk.WebKit.WebFrame
import qualified Graphics.UI.Gtk.WebKit.WebNavigationAction as NA
-- import Graphics.UI.Gtk.WebKit.WebSettings
import qualified Graphics.UI.Gtk.WebKit.WebView as WV
-- import System.Glib.GError

import LambdaCat.Configure
import LambdaCat.Supplier
import LambdaCat.UI
import LambdaCat.Utils.URI
import LambdaCat.View

-- |  Default WebView configuration.
webViewConf :: ViewConf WebView
webViewConf = WebViewConf

-- | Data type representing the view. Do not confuse this with WebKit's
-- WebView!
newtype WebView = WebView
    { webViewWidget :: WV.WebView  -- ^ The widget for the view.
    }

instance ViewClass WebView where
    data ViewConf WebView = WebViewConf
    new _ = do
        widget <- WV.webViewNew
        return WebView { webViewWidget = widget }

    embed wV@WebView { webViewWidget = widget } embedder callback = do
        -- Setup signal handling
        _ <- widget `on` WV.navigationPolicyDecisionRequested $
            \_wf nr na _wpd -> do
                muri <- NR.networkRequestGetUri nr
                reason <- NA.webNavigationActionGetReason na

                case (muri, reason) of
                    (Just _, NA.WebNavigationReasonFormResubmitted) ->
                        -- this is not handled because of the form data
                        return False

                    (Just uri, NA.WebNavigationReasonLinkClicked) -> do
                        supplyForView callback replaceView $ stringToURI uri
                        return True

                    _ ->
                        return False

        _ <- widget `on` WV.newWindowPolicyDecisionRequested $
            \_wf nr _na _wpd -> do
                muri <- NR.networkRequestGetUri nr

                case muri of
                    Just uri ->
                        supplyForView callback replaceView $ stringToURI uri

                    Nothing ->
                        return ()

                return True

        _ <- widget `on` WV.titleChanged  $ \_wf _title ->
            callback (updateView (View wV) TitleChanged)

        _ <- widget `on` WV.loadStarted   $ \_wf ->
            callback (updateView (View wV) URIChanged)

        _ <- widget `on` WV.loadCommitted $ \_wf ->
            callback (updateView (View wV) URIChanged)

        _ <- widget `on` WV.loadFinished  $ \_wf ->
            callback (updateView (View wV) URIChanged)

        _ <- widget `on` WV.progressChanged $ \progress ->
            callback (updateView (View wV) (ProgressChanged progress))

        -- Embed widget
        embedder $ castToWidget widget

    destroy WebView { webViewWidget = widget } = do
        -- TODO: Unref WebKit's WebView.
        -- WV.webViewLoadUri widget "about:blank"
        WV.webViewStopLoading widget
        widgetDestroy widget

    load WebView { webViewWidget = widget } uri = do
        -- TODO: Write module for URI conversion
        WV.webViewLoadUri widget $ show uri
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

