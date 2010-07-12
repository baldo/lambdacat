{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

module LambdaCat.Page.WebView 
    ( WebViewPage
    ) where

import LambdaCat.Page

import Control.Monad.Trans
import Data.Typeable
import Graphics.UI.Gtk.WebKit.WebView
import Network.URI

newtype WebViewPage = WebViewPage { unWebViewPage :: WebView }
  deriving Typeable

instance HasWidget WebViewPage WebView where
    getWidget = unWebViewPage

instance MonadIO m => PageClass WebViewPage m where 
    new = liftIO webViewNew >>= return . WebViewPage

    load page uri = liftIO $ webViewLoadUri (unWebViewPage page) uriString
        where uriString = uriToString id uri ""

    back    = liftIO . webViewGoBack . unWebViewPage
    forward = liftIO . webViewGoForward . unWebViewPage
    stop    = liftIO . webViewStopLoading . unWebViewPage
    reload  = liftIO . webViewReload . unWebViewPage
