{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module LambdaCat.Page.WebView 
    (
    ) where

import LambdaCat.Page

import Control.Monad.Trans
import Graphics.UI.Gtk.WebKit.WebView
import Network.URI

instance MonadIO m => PageClass WebView m where 
    new = liftIO webViewNew

    load page uri = liftIO $ webViewLoadUri page uriString
        where uriString = uriToString id uri ""

    back    = liftIO . webViewGoBack
    forward = liftIO . webViewGoForward
    stop    = liftIO . webViewStopLoading
    reload  = liftIO . webViewReload
