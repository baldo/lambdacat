{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, RankNTypes  #-}

module LambdaCat.UI.Glade where

import LambdaCat.Browser
import LambdaCat.Page
import LambdaCat.UI 

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.WebKit.WebView

import Control.Concurrent.MVar
import Control.Monad.Reader

data GladeUI = GladeUI {}

data GladeBrowser = GladeBrowser 
    { xml :: GladeXML
    , window :: Window
    , pageContainer :: Container 
    }

type GladeIO = ReaderT (MVar GladeUIState) IO 

data GladeUIState = GladeUIState {
        pages :: [WebView]
    }

runGladeIO :: GladeIO a -> IO a
runGladeIO f = do 
    state <- newMVar GladeUIState { pages = [] }
    runReaderT f state

withGladeUIState :: (GladeUIState -> GladeUIState) -> GladeIO ()
withGladeUIState f = do
    state <- ask 
    gladeUI <- liftIO $ takeMVar state 
    liftIO $ putMVar state $ f gladeUI 

askGladeUIState :: GladeIO GladeUIState
askGladeUIState = do 
    state <- ask 
    liftIO $ readMVar state

io :: forall a. IO a -> GladeIO a
io = liftIO

instance Browser GladeBrowser GladeIO

instance Page WebView GladeIO where 
    new = io $ do 
        webView <- webViewNew
        webViewLoadUri webView "http://www.haskell.org/"
        return webView

    back    = io . webViewGoBack
    forward = io . webViewGoForward
    stop    = io . webViewStopLoading
    reload  = io . webViewReload

instance UI GladeUI GladeBrowser WebView GladeIO where
    init = do
     _ <- io initGUI  
     return GladeUI {} 

    newBrowser _ = do 
        Just xml <- io $ xmlNew "lambdacat.glade"
        window <- io $ xmlGetWidget xml castToWindow "browserWindow"
        container <- io $ xmlGetWidget xml castToContainer "pageContainer"

        -- Events -------------------------------------------------------------
        io $ onDestroy window mainQuit

        io $ widgetShowAll window
        return GladeBrowser { xml = xml, window = window, pageContainer = container }

    embedPage _ GladeBrowser { xml = xml } webView = do
        scrolledWindow <- io $ xmlGetWidget xml castToScrolledWindow "pageScrolledWindow"
        io $ containerAdd scrolledWindow webView
        io $ widgetShowAll webView
        withGladeUIState (\ s -> s { pages = webView : (pages s) } )
        return ()

    mainLoop _ = do
        io mainGUI
        return ()
  
