{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, RankNTypes  #-}

module LambdaCat.UI.Glade where

import LambdaCat.Browser
import LambdaCat.Page
import LambdaCat.Page.WebView
import LambdaCat.UI 

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.WebKit.WebView

import Control.Concurrent.MVar
import Control.Monad.Reader

import Network.URI

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

io :: IO a -> GladeIO a
io = liftIO

gtkOn :: GObjectClass self => (self -> IO () -> IO (ConnectId self)) -> self -> GladeIO () -> GladeIO (ConnectId self) 
gtkOn onFunc widget func = do 
    state <- ask 
    io $ onFunc widget (runReaderT func state) 

instance BrowserClass GladeBrowser GladeIO

instance UIClass GladeUI GladeBrowser WebView GladeIO where
    init = do
     _ <- io initGUI  
     return GladeUI {} 

    newBrowser _ = do 
        Just xml <- io $ xmlNew "lambdacat.glade"
        window <- io $ xmlGetWidget xml castToWindow "browserWindow"
        container <- io $ xmlGetWidget xml castToContainer "pageContainer"

        -- General / Events ---------------------------------------------------
        io $ onDestroy window mainQuit

        -- Toolbar / Events ---------------------------------------------------
        let onTBC = gtkOn onToolButtonClicked
        pageBack <- xmlGetToolButton xml "pageBack"
        onTBC pageBack (pageAction back)
        pageForward <- xmlGetToolButton xml "pageForward"
        onTBC pageForward (pageAction forward)
        pageReload <- xmlGetToolButton xml "pageReload"
        onTBC pageReload (pageAction reload)
        pageURI <- io $ xmlGetWidget xml castToEntry "pageURI"
        gtkOn onEntryActivate pageURI $ do
            text <- io $ entryGetText pageURI 
            pageAction (\ w -> load w text)            
        
        io $ widgetShowAll window
        return GladeBrowser { xml = xml, window = window, pageContainer = container }

     where 
        pageAction :: (WebView -> GladeIO a) -> GladeIO ()
        pageAction f = do
            uiState <- askGladeUIState
            let (page:_) = pages uiState
            f page
            return ()

        xmlGetToolButton :: GladeXML -> String -> GladeIO ToolButton
        xmlGetToolButton xml name = io $ xmlGetWidget xml castToToolButton name  

    embedPage _ GladeBrowser { xml = xml } webView = do
        scrolledWindow <- io $ xmlGetWidget xml castToScrolledWindow "pageScrolledWindow"
        io $ containerAdd scrolledWindow webView
        io $ widgetShowAll webView
        withGladeUIState (\ s -> s { pages = webView : (pages s) } )
        return ()

    mainLoop _ = io mainGUI
