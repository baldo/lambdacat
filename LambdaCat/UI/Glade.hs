{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, RankNTypes  #-}

module LambdaCat.UI.Glade where

import LambdaCat.Browser
import LambdaCat.Page
import LambdaCat.Page.WebView
import LambdaCat.Page.Poppler
import LambdaCat.Page.MPlayer

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Control.Concurrent.MVar
import Control.Monad.Reader
import Network.URI

import qualified Data.Map as Map
import Data.Map (Map)

data GladeUI = GladeUI 
   { browsers  :: MVar (Map BrowserID (GladeBrowser,Map TabID (Page GladeIO)))
   }

data GladeBrowser = GladeBrowser 
    { gladeXml :: GladeXML
    , gladeWindow :: Window
    , pageContainer :: Notebook 
    }

instance Show GladeBrowser where
    show _ = "GladeBrowser"

type GladeIO = IO 

type TabID = Int

runGladeIO :: GladeIO a -> IO a
runGladeIO = id

addBrowser :: GladeUI -> GladeBrowser -> GladeIO BrowserID
addBrowser ui g = do
    let bs = browsers ui
    bid <- newBrowserID
    liftIO $ modifyMVar_ bs (return . Map.insert bid (g,Map.empty))
    return bid

-- | Add a page to the browser identified by BrowserID, if there
-- is already an page with given TabID then the page gets replaced.
addPageToBrowser :: GladeUI -> BrowserID -> TabID -> Page GladeIO ->  GladeIO ()
addPageToBrowser ui bid tid page = 
  liftIO $ modifyMVar_ (browsers ui) (return . Map.update (\ (bw,pages) -> Just (bw,Map.insert tid page pages)) bid)

removePageFromBrowser :: GladeUI -> BrowserID -> Page GladeIO -> GladeIO()
removePageFromBrowser ui bid page = 
    liftIO $ modifyMVar_ (browsers ui) (return . Map.update (\ (bw,pages) -> Just (bw,Map.filter (/=page) pages)) bid)


replacePageInBrowser :: GladeUI -> BrowserID -> Page GladeIO -> Page GladeIO -> GladeIO ()
replacePageInBrowser ui bid oldpage newpage = do
    liftIO $ modifyMVar_ (browsers ui) (return . Map.update (\ (bw,pages) -> Just (bw,Map.map replace pages)) bid)
  where replace :: Page GladeIO -> Page GladeIO
        replace page | page == oldpage = newpage
                     | otherwise       = page 

countTabsInBrowser :: GladeUI -> BrowserID -> GladeIO Int
countTabsInBrowser ui bid =
    liftIO $ withMVar (browsers ui) (return . size . Map.lookup bid)
  where size (Just (_,m)) = Map.size m
        size Nothing  = 0

getTabIDForPage :: GladeUI -> BrowserID -> Page GladeIO -> GladeIO (Maybe Int) 
getTabIDForPage ui bid page = 
    liftIO $ withMVar (browsers ui) (return . selectTabID . Map.lookup bid)
  where selectTabID Nothing      = Nothing
        selectTabID (Just (_,m)) = Map.foldWithKey 
            (\ k page' s -> case s of 
                o@(Just _) -> o
                Nothing -> if page == page' then (Just k) else Nothing ) Nothing m

getPageFromBrowser :: GladeUI -> BrowserID -> TabID -> GladeIO (Maybe (Page GladeIO))
getPageFromBrowser ui bid tid =
    liftIO $ withMVar (browsers ui) (return . getPage . Map.lookup bid)
  where getPage (Just (_,m)) = Map.lookup tid m
        getPage Nothing = Nothing 

removeBrowser :: GladeUI -> BrowserID -> GladeIO ()
removeBrowser ui bid = liftIO $ modifyMVar_ (browsers ui) (return . Map.delete bid)

getBrowser :: GladeUI -> BrowserID -> GladeIO (Maybe GladeBrowser)
getBrowser ui bid =  do
    let bs = browsers ui
    -- TODO error handling
    b <- liftIO $ withMVar bs (return . Map.lookup bid)
    case b of 
        (Just x) -> return $ Just (fst x)
        Nothing  -> return Nothing 

getBrowserPages :: GladeUI -> BrowserID -> GladeIO [Page GladeIO]
getBrowserPages ui bid = do 
    let bs = browsers ui
    -- TODO error handling
    mPages  <- liftIO $ withMVar bs (return . Map.lookup bid)
    case mPages of 
        Just b -> return $ Map.elems (snd  b)
        Nothing -> return [] 

getBrowserByPage :: GladeUI -> Page GladeIO -> GladeIO (Maybe (BrowserID,GladeBrowser))
getBrowserByPage ui page = do
    let bs = browsers ui 
    liftIO $ withMVar bs (return . Map.foldWithKey findBrowser Nothing)
 where findBrowser _ _ x@(Just _) = x
       findBrowser bid (browser,pMap) Nothing =
            let page' = Map.filter (== page) pMap
            in if Map.null page'
             then Nothing
             else Just (bid,browser)

io :: IO a -> GladeIO a
io = liftIO

gtkOn :: GObjectClass self => (self -> IO () -> IO (ConnectId self)) -> self -> GladeIO () -> GladeIO (ConnectId self) 
gtkOn onFunc widget func = do 
    sink <- getSink 
    io $ onFunc widget $ sink func

instance UIClass GladeUI GladeIO where
    init = do
     _ <- io initGUI  
     b <- io $ newMVar Map.empty 
     return GladeUI { browsers = b} 

    newBrowser ui = do 
        Just xml <- io $ xmlNew "lambdacat.glade"
        window <- io $ xmlGetWidget xml castToWindow "browserWindow"
        notebook <- io $ xmlGetWidget xml castToNotebook "pageNoteBook"

        let browser = GladeBrowser { gladeXml = xml, gladeWindow = window, pageContainer = notebook }
        bid <- addBrowser ui browser 
        -- General / Events ---------------------------------------------------
        _ <- io $ onDestroy window mainQuit
        _ <- io $ notebook `on`  switchPage $ \ newActive -> do
            mPage <- io $ getPageFromBrowser ui bid newActive
            case mPage of
                Nothing  -> return ()
                (Just p) -> do 
                    uriChanged ui p
                    changedTitle ui p 

        -- Toolbar / Events ---------------------------------------------------
        let onTBC w a = gtkOn onToolButtonClicked w a >> return ()
        pageBack <- xmlGetToolButton xml "pageBack"
        onTBC pageBack (pageAction notebook bid back)
        pageForward <- xmlGetToolButton xml "pageForward"
        onTBC pageForward (pageAction notebook bid forward)
        pageReload <- xmlGetToolButton xml "pageReload"
        onTBC pageReload (pageAction notebook bid reload)
        pageURI <- io $ xmlGetWidget xml castToEntry "pageURI"
        _ <- gtkOn onEntryActivate pageURI $ do
            text <- io $ entryGetText pageURI
            case parseURI text of
              Just uri ->
                pageAction notebook bid (\ w -> do
                                let pageList = [ (Page (undefined :: WebViewPage), ["http:","https:"])
                                               , (Page (undefined :: PopplerPage), ["file:"])
                                               , (Page (undefined :: MPlayerPage), ["mms:"])
                                               ]
                                Just w' <- pageFromProtocol (update ui)  pageList (Just w) (Just uri)
                                replacePage ui bid w w'
                                load w' uri)
              Nothing -> return ()
        io $ widgetShowAll window
        return bid 

     where 
        pageAction :: Notebook -> BrowserID -> (Page GladeIO -> GladeIO a) -> GladeIO ()
        pageAction notebook bid f = do
            -- TODO select correct page
            tid <- notebookGetCurrentPage notebook
            mPage <- getPageFromBrowser ui bid tid 
            case mPage of
                Just p  -> f p >> return ()
                Nothing -> return ()

        xmlGetToolButton :: GladeXML -> String -> GladeIO ToolButton
        xmlGetToolButton xml name = io $ xmlGetWidget xml castToToolButton name  

    update ui f = do
        f ui 
        return ()

    uriChanged ui page = do 
        p <- getBrowserByPage ui page
        case p of
          Just (_,GladeBrowser { gladeXml = xml }) -> do
            pageURI <- liftIO $ xmlGetWidget xml castToEntry "pageURI"
            uri <- getCurrentURI page 
            liftIO $ entrySetText pageURI (uriString uri)
          Nothing  -> return ()
      where uriString uri = uriToString id uri ""
    
    changedTitle ui page = do 
        mBrowser <- getBrowserByPage ui page
        case mBrowser of
            Nothing -> return ()
            Just (bid,GladeBrowser { gladeXml = xml}) -> do
                title <- getCurrentTitle page 
                mTid  <- getTabIDForPage ui bid page
                case mTid of
                    Nothing  -> return ()
                    Just tid -> do
                        notebook <- io $ xmlGetWidget xml castToNotebook "pageNoteBook"
                        mTab <- io $ notebookGetNthPage notebook tid 
                        case mTab of 
                            (Just tab) -> io $ notebookSetTabLabelText notebook tab title 
                            Nothing    -> return ()
                window <- io $ xmlGetWidget xml castToWindow "browserWindow"
                io $ set window [ windowTitle := title ] 

    replacePage ui bid oldpage page@(Page hasWidget) = do
        bool <- getBrowser ui bid  
        case bool of 
          Just (GladeBrowser { gladeXml = xml }) -> do
            let widget = getWidget hasWidget
            noteBook <- io $ xmlGetWidget xml castToNotebook "pageNoteBook"
            -- Replace page in container
            maybeTabID <- getTabIDForPage ui bid oldpage
            case maybeTabID of 
              Just tabID -> do
                io $ do
                    -- tabID <- notebookGetCurrentPage noteBook
                    (Just scroll) <- notebookGetNthPage noteBook tabID
                    let scrolledWindow =  castToScrolledWindow scroll
                    mapM_ (containerRemove scrolledWindow) =<< containerGetChildren scrolledWindow 
                    containerAdd scrolledWindow widget
                    widgetShowAll widget
                -- Replace page in state/model
                replacePageInBrowser ui bid oldpage page
                return ()
              Nothing -> return ()
          Nothing -> return ()

    embedPage ui bid page@(Page hasWidget) = do
        bool <- getBrowser ui bid
        case bool of 
          Just (GladeBrowser { gladeXml = xml }) -> do
            let widget = getWidget hasWidget
            noteBook  <- io $ xmlGetWidget xml castToNotebook "pageNoteBook"
            scrolledWindow <- io $ scrolledWindowNew Nothing Nothing
            io $ containerAdd scrolledWindow widget 
            newTabID  <- io $ notebookAppendPage noteBook scrolledWindow "Foo"
            widgetShowAll noteBook
            addPageToBrowser ui bid newTabID page
            return ()
          Nothing -> return ()

    mainLoop _ = io mainGUI

instance SinkMonad GladeIO where
    getSink = return liftIO 
