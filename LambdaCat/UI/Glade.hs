{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, RankNTypes  #-}

module LambdaCat.UI.Glade where

import LambdaCat.Browser
import LambdaCat.Page.WebView
import LambdaCat.Page.Poppler
import LambdaCat.Page

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

import Control.Concurrent.MVar
import Control.Monad.Reader

import Network.URI

import qualified Data.Map as Map
import Data.Map (Map)

data GladeUI = GladeUI {
    browsers :: MVar (Map BrowserID GladeBrowser)
}

data GladeBrowser = GladeBrowser 
    { gladeXml :: GladeXML
    , gladeWindow :: Window
    , pageContainer :: Container 
    }

instance Show GladeBrowser where
    show _ = "GladeBrowser"

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

gAddBrowser :: GladeUI -> GladeBrowser -> GladeIO BrowserID
gAddBrowser ui g = do
    let bs = browsers ui
    bid <- newBrowserID
    liftIO $ modifyMVar_ bs (return . Map.insert bid g)
    return bid

gRemoveBrowser :: GladeUI -> BrowserID -> GladeIO ()
gRemoveBrowser ui bid = do
    let bs = browsers ui
    bid <- newBrowserID
    liftIO $ modifyMVar_ bs (return . Map.delete bid)

gGetBrowser :: GladeUI -> BrowserID -> GladeIO GladeBrowser
gGetBrowser ui bid =  do
    let bs = browsers ui
    -- TODO error handling
    (Just b) <- liftIO $ withMVar bs (return . Map.lookup bid)
    return b

io :: IO a -> GladeIO a
io = liftIO

gtkOn :: GObjectClass self => (self -> IO () -> IO (ConnectId self)) -> self -> GladeIO () -> GladeIO (ConnectId self) 
gtkOn onFunc widget func = do 
    state <- ask 
    io $ onFunc widget (runReaderT func state) 

instance UIClass GladeUI GladeIO where
    init = do
     _ <- io initGUI  
     b <- io $ newMVar Map.empty 
     return GladeUI { browsers = b } 

    newBrowser ui = do 
        Just xml <- io $ xmlNew "lambdacat.glade"
        window <- io $ xmlGetWidget xml castToWindow "browserWindow"
        container <- io $ xmlGetWidget xml castToContainer "pageContainer"

        let browser = GladeBrowser { gladeXml = xml, gladeWindow = window, pageContainer = container }
        bid <- gAddBrowser ui browser 
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
                                Just w' <- pageFromProtocol (update ui)  pageList (Just w) (Just uri)
                                load w' uri
                                embedPage ui bid w')
        io $ widgetShowAll window
        return bid 

     where 
        pageAction :: (Page GladeIO -> GladeIO a) -> GladeIO ()
        pageAction f = do
            uiState <- askGladeUIState
            let (page:_) = pages uiState
            _ <- f page
            return ()

        xmlGetToolButton :: GladeXML -> String -> GladeIO ToolButton
        xmlGetToolButton xml name = io $ xmlGetWidget xml castToToolButton name  

    update ui f = do
        f ui 
        return ()

    uriChanged ui page = do 
        let bs = browsers ui   
        GladeBrowser { gladeXml = xml } <- liftIO $ withMVar bs (\ x -> return . head $ Map.elems x)
        pageURI <- liftIO $ xmlGetWidget xml castToEntry "pageURI"
        uri <- getCurrentURI page 
        liftIO $ print uri
        liftIO $ entrySetText pageURI (uriString uri)
      where uriString uri = uriToString id uri ""
        

    embedPage ui bid page@(Page hasWidget) = do
        GladeBrowser { gladeXml = xml } <- gGetBrowser ui bid
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

instance SinkMonad GladeIO where
    getSink = do 
        state <- ask
        return (\ f -> liftIO $ runReaderT f state)
