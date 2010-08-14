{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, RankNTypes  #-}

module LambdaCat.UI.Glade where

import LambdaCat.Browser
import LambdaCat.Page
import LambdaCat.Page.Cat
import LambdaCat.Configure (lambdaCatConf,LambdaCatConf (..))
import LambdaCat.Page.WebView
import LambdaCat.Page.Poppler
import LambdaCat.Page.MPlayer

import Paths_lambdacat

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Control.Concurrent.MVar
import Network.URI

import qualified Data.Map as Map
import Data.Map (Map)

data GladeUI = GladeUI 
   { browsers  :: MVar (Map BrowserID (GladeBrowser,Map TabID Page))
   }

data GladeBrowser = GladeBrowser 
    { gladeXml :: GladeXML
    , gladeWindow :: Window
    , pageContainer :: Notebook 
    }

instance Show GladeBrowser where
    show _ = "GladeBrowser"

type TabID = Int

addBrowser :: GladeUI -> GladeBrowser -> IO BrowserID
addBrowser ui g = do
    let bs = browsers ui
    bid <- newBrowserID
    modifyMVar_ bs (return . Map.insert bid (g,Map.empty))
    return bid

-- | Add a page to the browser identified by BrowserID, if there
-- is already an page with given TabID then the page gets replaced.
addPageToBrowser :: GladeUI -> BrowserID -> TabID -> Page ->  IO ()
addPageToBrowser ui bid tid page = 
  modifyMVar_ (browsers ui) (return . Map.update (\ (bw,pages) -> Just (bw,Map.insert tid page pages)) bid)

removePageFromBrowser :: GladeUI -> BrowserID -> Page -> IO ()
removePageFromBrowser ui bid page = 
  modifyMVar_ (browsers ui) (return . Map.update (\ (bw,pages) -> Just (bw,Map.filter (/=page) pages)) bid)


replacePageInBrowser :: GladeUI -> BrowserID -> Page -> Page -> IO ()
replacePageInBrowser ui bid oldpage newpage = do
    modifyMVar_ (browsers ui) (return . Map.update (\ (bw,pages) -> Just (bw,Map.map replace pages)) bid)
  where replace :: Page -> Page 
        replace page | page == oldpage = newpage
                     | otherwise       = page 

countTabsInBrowser :: GladeUI -> BrowserID -> IO Int
countTabsInBrowser ui bid =
    withMVar (browsers ui) (return . size . Map.lookup bid)
  where size (Just (_,m)) = Map.size m
        size Nothing  = 0

getTabIDForPage :: GladeUI -> BrowserID -> Page -> IO (Maybe Int) 
getTabIDForPage ui bid page = 
    withMVar (browsers ui) (return . selectTabID . Map.lookup bid)
  where selectTabID Nothing      = Nothing
        selectTabID (Just (_,m)) = Map.foldWithKey 
            (\ k page' s -> case s of 
                o@(Just _) -> o
                Nothing -> if page == page' then (Just k) else Nothing ) Nothing m

getPageFromBrowser :: GladeUI -> BrowserID -> TabID -> IO (Maybe Page)
getPageFromBrowser ui bid tid =
    withMVar (browsers ui) (return . getPage . Map.lookup bid)
  where getPage (Just (_,m)) = Map.lookup tid m
        getPage Nothing = Nothing 

removeBrowser :: GladeUI -> BrowserID -> IO ()
removeBrowser ui bid = modifyMVar_ (browsers ui) (return . Map.delete bid)

getBrowser :: GladeUI -> BrowserID -> IO (Maybe GladeBrowser)
getBrowser ui bid =  do
    let bs = browsers ui
    -- TODO error handling
    b <- withMVar bs (return . Map.lookup bid)
    case b of 
        (Just x) -> return $ Just (fst x)
        Nothing  -> return Nothing 

getBrowserPages :: GladeUI -> BrowserID -> IO [Page]
getBrowserPages ui bid = do 
    let bs = browsers ui
    -- TODO error handling
    mPages  <- withMVar bs (return . Map.lookup bid)
    case mPages of 
        Just b -> return $ Map.elems (snd  b)
        Nothing -> return [] 

getBrowserByPage :: GladeUI -> Page -> IO (Maybe (BrowserID,GladeBrowser))
getBrowserByPage ui page = do
    let bs = browsers ui 
    withMVar bs (return . Map.foldWithKey findBrowser Nothing)
 where findBrowser _ _ x@(Just _) = x
       findBrowser bid (browser,pMap) Nothing =
            let page' = Map.filter (== page) pMap
            in if Map.null page'
             then Nothing
             else Just (bid,browser)

instance UIClass GladeUI where
    init = do
     _ <- initGUI  
     b <- newMVar Map.empty 
     return GladeUI { browsers = b} 

    newBrowser ui = do 
        fpath    <- getDataFileName "lambdacat.glade"
        Just xml <- xmlNew fpath
        window   <- xmlGetWidget xml castToWindow "browserWindow"
        notebook <- xmlGetWidget xml castToNotebook "pageNoteBook"

        let browser = GladeBrowser { gladeXml = xml, gladeWindow = window, pageContainer = notebook }
        bid <- addBrowser ui browser 
        -- General / Events ---------------------------------------------------
        _ <- onDestroy window mainQuit
        _ <- notebook `on`  switchPage $ \ newActive -> do
            mPage <- getPageFromBrowser ui bid newActive
            case mPage of
                Nothing  -> return ()
                (Just p) -> do 
                    uriChanged ui p
                    changedTitle ui p 

        -- Toolbar / Events ---------------------------------------------------
        pageBack <- xmlGetToolButton xml "pageBack"
        onToolButtonClicked pageBack (pageAction notebook bid back)
        pageForward <- xmlGetToolButton xml "pageForward"
        onToolButtonClicked pageForward (pageAction notebook bid forward)
        pageReload <- xmlGetToolButton xml "pageReload"
        onToolButtonClicked pageReload (pageAction notebook bid reload)
        pageURI <- xmlGetWidget xml castToEntry "pageURI"
        _ <- onEntryActivate pageURI  $ do
            text <- entryGetText pageURI
            case parseURI text of
              Just uri ->
                pageAction notebook bid (\ w -> do
                                mw' <- pageFromProtocol (update ui bid)  (pageList lambdaCatConf) (Just w) (Just uri)
                                case mw' of 
                                    -- TODO call an default error page
                                    Nothing -> return ()
                                    Just w' -> do
                                        replacePage ui bid w w'
                                        load w' uri
                                        return ()
                                )
              Nothing -> return ()
        widgetShowAll window
        return bid 

     where 
        pageAction :: Notebook -> BrowserID -> (Page -> IO a) -> IO ()
        pageAction notebook bid f = do
            -- TODO select correct page
            tid <- notebookGetCurrentPage notebook
            mPage <- getPageFromBrowser ui bid tid 
            case mPage of
                Just p  -> f p >> return ()
                Nothing -> return ()

        xmlGetToolButton :: GladeXML -> String -> IO ToolButton
        xmlGetToolButton xml name = xmlGetWidget xml castToToolButton name  

    update ui bid f = do
        f ui bid  
        return ()

    uriChanged ui page = do 
        p <- getBrowserByPage ui page
        case p of
          Just (_,GladeBrowser { gladeXml = xml }) -> do
            pageURI <- xmlGetWidget xml castToEntry "pageURI"
            uri <- getCurrentURI page 
            entrySetText pageURI (uriString uri)
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
                        notebook <- xmlGetWidget xml castToNotebook "pageNoteBook"
                        mTab <- notebookGetNthPage notebook tid 
                        case mTab of 
                            (Just tab) -> notebookSetTabLabelText notebook tab title 
                            Nothing    -> return ()
                window <- xmlGetWidget xml castToWindow "browserWindow"
                set window [ windowTitle := title ] 

    replacePage ui bid oldpage page@(Page hasWidget) = do
        bool <- getBrowser ui bid  
        case bool of 
          Just (GladeBrowser { gladeXml = xml }) -> do
            let widget = getWidget hasWidget
            noteBook <- xmlGetWidget xml castToNotebook "pageNoteBook"
            -- Replace page in container
            maybeTabID <- getTabIDForPage ui bid oldpage
            case maybeTabID of 
              Just tabID -> do
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
            noteBook  <- xmlGetWidget xml castToNotebook "pageNoteBook"
            scrolledWindow <- scrolledWindowNew Nothing Nothing
            containerAdd scrolledWindow widget 
            newTabID  <- notebookAppendPage noteBook scrolledWindow "Foo"
            widgetShowAll noteBook
            addPageToBrowser ui bid newTabID page
            return ()
          Nothing -> return ()

    mainLoop _ = mainGUI
