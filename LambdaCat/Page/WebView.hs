{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

module LambdaCat.Page.WebView 
    ( WebViewPage
    ) where

import LambdaCat.Page

import Control.Monad.Trans
import Data.Typeable
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk
import Network.URI
import Control.Monad

newtype WebViewPage = WebViewPage { unWebViewPage :: WebView }
  deriving Typeable

instance HasWidget WebViewPage WebView where
    getWidget = unWebViewPage

instance SinkMonad m => PageClass WebViewPage m where 
    new cb = do
        page <- liftIO webViewNew >>= return . WebViewPage
        cb (\ui -> uriChanged ui (Page page))
        sink <- getSink
        liftIO $ (getWidget page) `on` loadFinished $ (\ _ -> sink $ cb (\ui -> uriChanged ui (Page page)))
        return page
        

    load page uri = liftIO $ webViewLoadUri (unWebViewPage page) uriString
        where uriString = uriToString id uri ""

    getCurrentURI page = do 
        muri <- liftIO $ webViewGetUri $  unWebViewPage page
        case muri of
            Just uri -> case parseURI uri of 
                Just x -> return x
                _ -> return nullURI
            _ -> return nullURI

    back    = liftIO . webViewGoBack . unWebViewPage
    forward = liftIO . webViewGoForward . unWebViewPage
    stop    = liftIO . webViewStopLoading . unWebViewPage
    reload  = liftIO . webViewReload . unWebViewPage
