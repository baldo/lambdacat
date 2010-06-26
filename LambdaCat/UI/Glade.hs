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
    { window :: Window
    , pageContainer :: Container 
    }

instance Browser GladeBrowser IO

instance Page WebView IO where 
    new = webViewNew

instance UI GladeUI GladeBrowser WebView IO where
    init = do
     _ <- initGUI  
     return GladeUI {} 

    newBrowser _ = do 
        Just xml <- xmlNew "lambdacat.glade"
        window <- xmlGetWidget xml castToWindow "browserWindow"
        container <- xmlGetWidget xml castToContainer "pageContainer"
        widgetShowAll window
        return GladeBrowser { window = window, pageContainer = container }

    embedPage _ GladeBrowser { pageContainer = container }  webView = do
        containerAdd container webView
        return ()

    mainLoop _ = do
        mainGUI
        return ()
  
