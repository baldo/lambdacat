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

newtype WebViewPage = WebViewPage { unWebViewPage :: WebView }
  deriving (Eq, Typeable)

instance HasWidget WebViewPage WebView where
    getWidget = unWebViewPage

instance SinkMonad m => PageClass WebViewPage m where 
    new cb = do
        page <- liftIO webViewNew >>= return . WebViewPage
        cb (\ui _ -> uriChanged ui (Page page))
        sink <- getSink
        _ <- liftIO $ getWidget page `on` loadFinished $ (\ _ -> sink $ cb (\ui _ -> uriChanged ui (Page page)))
        _ <- liftIO $ getWidget page `on` loadFinished $ (\ _ -> sink $ cb (\ui _ -> changedTitle ui (Page page)))
        _ <- liftIO $ getWidget page `on` webViewReady $ (do  sink $ cb (\ui bid -> embedPage ui bid (Page page)) ; return True; )
        return page
        

    load page uri = liftIO $ webViewLoadUri (unWebViewPage page) uriString >> return True
        where uriString = uriToString id uri ""

    getCurrentURI page = do 
        muri <- liftIO.webViewGetUri.unWebViewPage $ page
        case muri of
            Just uri -> case parseURI uri of 
                Just x -> return x
                _ -> return nullURI
            _ -> return nullURI

    getCurrentTitle page = do
        mTitle <- liftIO.webViewGetTitle.unWebViewPage $ page
        case mTitle of
            Just title -> return title
            _ -> return ""

    back    = liftIO . webViewGoBack . unWebViewPage
    forward = liftIO . webViewGoForward . unWebViewPage
    stop    = liftIO . webViewStopLoading . unWebViewPage
    reload  = liftIO . webViewReload . unWebViewPage
