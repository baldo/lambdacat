{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module LambdaCat.UI.Glade where

import LambdaCat.Browser
import LambdaCat.Page
import LambdaCat.UI 

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.WebKit.WebView

data GladeUI = GladeUI {}

data GladeBrowser = GladeBrowser 
    { xml :: GladeXML
    , window :: Window
    , pageContainer :: Container 
    }

instance Browser GladeBrowser IO

instance Page WebView IO where 
    new = do 
        webView <- webViewNew
        webViewLoadUri webView "http://www.haskell.org/"
        return webView

    back    = webViewGoBack
    forward = webViewGoForward
    stop    = webViewStopLoading
    reload  = webViewReload

instance UI GladeUI GladeBrowser WebView IO where
    init = do
     _ <- initGUI  
     return GladeUI {} 

    newBrowser _ = do 
        Just xml <- xmlNew "lambdacat.glade"
        window <- xmlGetWidget xml castToWindow "browserWindow"
        container <- xmlGetWidget xml castToContainer "pageContainer"

        -- Events -------------------------------------------------------------
        onDestroy window mainQuit

        widgetShowAll window
        return GladeBrowser { xml = xml, window = window, pageContainer = container }

    embedPage _ GladeBrowser { xml = xml }  webView = do
        scrolledWindow <- xmlGetWidget xml castToScrolledWindow "pageScrolledWindow"
        containerAdd scrolledWindow webView
        widgetShowAll webView
        return ()

    mainLoop _ = do
        mainGUI
        return ()
  
