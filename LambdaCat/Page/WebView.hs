{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

module LambdaCat.Page.WebView 
    ( WebViewPage

    , webViewPage
    ) where

import LambdaCat.Page

import Data.Typeable
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk
import Network.URI

newtype WebViewPage = WebViewPage { unWebViewPage :: WebView }
  deriving (Eq, Typeable)

instance HasWidget WebViewPage WebView where
    getWidget = unWebViewPage

webViewPage :: Page 
webViewPage = Page (undefined :: WebViewPage)

instance PageClass WebViewPage where 
    new cb = do
        page <- webViewNew >>= return . WebViewPage
        cb (\ui _ -> uriChanged ui (Page page))
        let widget = getWidget page 
        _ <- widget `on` loadFinished  $ (\ _ -> cb (\ui _ -> uriChanged ui (Page page)))
        _ <- widget `on` loadFinished  $ (\ _ -> cb (\ui _ -> changedTitle ui (Page page)))
        _ <- widget `on` createWebView $ (\ _ -> createNew >>= return . unWebViewPage)
        return page
      where createNew :: IO WebViewPage
            createNew = do
                page <- new cb
                cb (\ ui bid -> embedPage ui bid (Page page))
                return page 
        

    load page uri =  webViewLoadUri (unWebViewPage page) uriString >> return True
        where uriString = uriToString id uri ""

    getCurrentURI page = do 
        muri <- webViewGetUri.unWebViewPage $ page
        case muri of
            Just uri -> case parseURI uri of 
                Just x -> return x
                _ -> return nullURI
            _ -> return nullURI

    getCurrentTitle page = do
        mTitle <- webViewGetTitle.unWebViewPage $ page
        case mTitle of
            Just title -> return title
            _ -> return ""

    back    =  webViewGoBack . unWebViewPage
    forward =  webViewGoForward . unWebViewPage
    stop    =  webViewStopLoading . unWebViewPage
    reload  =  webViewReload . unWebViewPage
