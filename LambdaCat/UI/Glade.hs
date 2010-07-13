{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, RankNTypes  #-}

module LambdaCat.UI.Glade where

import LambdaCat.Browser
import LambdaCat.Core
import LambdaCat.Page
import LambdaCat.Page.WebView
import LambdaCat.Page.Poppler
import LambdaCat.UI 

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

import Control.Concurrent.MVar
import Control.Monad.Reader

import Network.URI

data GladeUI = GladeUI {}

data GladeBrowser = GladeBrowser 
    { gladeXml :: GladeXML
    , gladeWindow :: Window
    , pageContainer :: Container 
    }

type GladeIO = ReaderT (MVar GladeUIState) IO 

data GladeUIState = GladeUIState {
        pages :: [Page GladeIO]
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

instance UIClass GladeUI GladeBrowser (Page GladeIO) GladeIO where
    init = do
     _ <- io initGUI  
     return GladeUI {} 

    newBrowser _ = do 
        Just xml <- io $ xmlNew "lambdacat.glade"
        window <- io $ xmlGetWidget xml castToWindow "browserWindow"
        container <- io $ xmlGetWidget xml castToContainer "pageContainer"

        -- General / Events ---------------------------------------------------
        _ <- io $ onDestroy window mainQuit

        -- Toolbar / Events ---------------------------------------------------
        let onTBC w a = gtkOn onToolButtonClicked w a >> return ()
        pageBack <- xmlGetToolButton xml "pageBack"
        onTBC pageBack (pageAction back)
        pageForward <- xmlGetToolButton xml "pageForward"
        onTBC pageForward (pageAction forward)
        pageReload <- xmlGetToolButton xml "pageReload"
        onTBC pageReload (pageAction reload)
        pageURI <- io $ xmlGetWidget xml castToEntry "pageURI"
        _ <- gtkOn onEntryActivate pageURI $ do
            text <- io $ entryGetText pageURI
            let (Just uri) = parseURI text
            pageAction (\ w -> do
                                let pageList = [ (Page (undefined :: WebViewPage), ["http:","https:"])
                                               , (Page (undefined :: PopplerPage), ["file:"])
                                               ]
                                Just w' <- pageFromProtocol pageList (Just w) (Just uri)
                                load w' uri
                                embedPage GladeUI {} GladeBrowser { gladeXml = xml } w')
        
        io $ widgetShowAll window
        lambdaCatAddUI (UI GladeUI {})
        return GladeBrowser { gladeXml = xml, gladeWindow = window, pageContainer = container }

     where 
        pageAction :: (Page GladeIO -> GladeIO a) -> GladeIO ()
        pageAction f = do
            uiState <- askGladeUIState
            let (page:_) = pages uiState
            _ <- f page
            return ()

        xmlGetToolButton :: GladeXML -> String -> GladeIO ToolButton
        xmlGetToolButton xml name = io $ xmlGetWidget xml castToToolButton name  

    embedPage _ GladeBrowser { gladeXml = xml } page@(Page hasWidget) = do
        let widget = getWidget hasWidget
        scrolledWindow <- io $ xmlGetWidget xml castToScrolledWindow "pageScrolledWindow"
        io $ do
            ws <- containerGetChildren scrolledWindow
            mapM_ (containerRemove scrolledWindow) ws
            containerAdd scrolledWindow widget
            widgetShowAll widget
        withGladeUIState (\ s -> s { pages = page : (pages s) } )
        return ()

    mainLoop _ = io mainGUI
