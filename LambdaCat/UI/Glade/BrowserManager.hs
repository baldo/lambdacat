module LambdaCat.UI.Glade.BrowserManager 
    ( BrowserManager
    , GladeBrowser (..)

    , newBrowserManager
    
    -- adding functions
    , addBrowser
    , addPageToBrowser

    -- updating function
    , replacePageInBrowser

    -- destructive functions
    , removePageFromBrowser
    , removeBrowser

    -- lookup functions
    , getPageFromBrowser
    , getBrowser
    , getBrowserPages
    , getBrowserByPage
    , countTabsInBrowser
    , getContainerForPage
    ) where

import LambdaCat.Browser (BrowserId, newBrowserId)
import LambdaCat.Page
import LambdaCat.UI.Glade.PersistentTabId (TabId) 

import Control.Concurrent.MVar
import Data.Map (Map)
import qualified Data.Map as Map
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

data GladeBrowser = GladeBrowser 
    { gladeXml      :: GladeXML
    , gladeWindow   :: Window
    , pageContainer :: Notebook 
    }

instance Show GladeBrowser where
    show _ = "GladeBrowser"
newtype BrowserManager = BM { unBM :: MVar BrowserMap }

type BrowserMap = Map BrowserId (GladeBrowser,TabMap)
type TabMap = Map TabId (Container,Page)

newBrowserManager :: IO BrowserManager
newBrowserManager = do
    m <- newMVar Map.empty
    return (BM m)

addBrowser :: BrowserManager -> GladeBrowser -> IO BrowserId
addBrowser (BM bm) g = do
    bid <- newBrowserId
    modifyMVar_ bm (return . Map.insert bid (g,Map.empty))
    return bid

-- | Add a (container,page) to the browser identified by BrowserId, if there
-- is already a page with given TabId then the page gets replaced.
addPageToBrowser :: BrowserManager -> BrowserId -> TabId -> Container -> Page -> IO ()
addPageToBrowser (BM bm) bid tid container page = 
  modifyMVar_ bm (return . Map.update (\ (bw,pages) -> Just (bw,Map.insert tid (container,page) pages)) bid)

removePageFromBrowser :: BrowserManager -> BrowserId -> Page -> IO ()
removePageFromBrowser (BM bm) bid page = 
  modifyMVar_ bm (return . Map.update (\ (bw,pages) -> Just (bw,Map.filter ((/= page).snd) pages)) bid)

replacePageInBrowser :: BrowserManager -> BrowserId -> Page -> Page -> IO ()
replacePageInBrowser (BM bm) bid oldpage newpage = do
    modifyMVar_ bm (return . Map.update (\ (bw,pages) -> Just (bw,Map.map replace pages)) bid)
  where replace :: (Container,Page) -> (Container,Page) 
        replace (container,page) | page == oldpage = (container,newpage)
                                 | otherwise       = (container,page)

countTabsInBrowser :: BrowserManager -> BrowserId -> IO Int
countTabsInBrowser (BM bm) bid =
    withMVar bm (return . size . Map.lookup bid)
  where size (Just (_,m)) = Map.size m
        size Nothing  = 0

{- Don't know if we need this function after refactoring
getTabIdForPage :: BrowserManager -> BrowserId -> Page -> IO (Maybe Int) 
getTabIdForPage (BM bm) bid page = 
    withMVar bm (return . selectTabId . Map.lookup bid)
  where selectTabId Nothing      = Nothing
        selectTabId (Just (_,m)) = Map.foldWithKey 
            (\ k page' s -> case s of 
                o@(Just _) -> o
                Nothing -> if page == page' then (Just k) else Nothing ) Nothing m
-}

getPageFromBrowser :: BrowserManager -> BrowserId -> TabId -> IO (Maybe (Container,Page))
getPageFromBrowser (BM bm) bid tid =
    withMVar bm (return . getPage . Map.lookup bid)
  where getPage (Just (_,m)) = Map.lookup tid m
        getPage Nothing      = Nothing 

removeBrowser :: BrowserManager -> BrowserId -> IO ()
removeBrowser (BM bm) bid = modifyMVar_ bm (return . Map.delete bid)

getBrowser :: BrowserManager -> BrowserId -> IO (Maybe GladeBrowser)
getBrowser (BM bm) bid =  do
    -- TODO error handling
    b <- withMVar bm (return . Map.lookup bid)
    case b of 
        (Just x) -> return $ Just (fst x)
        Nothing  -> return Nothing

getBrowserPages :: BrowserManager -> BrowserId -> IO [(Container,Page)]
getBrowserPages (BM bm) bid = do 
    -- TODO error handling
    mPages  <- withMVar bm (return . Map.lookup bid)
    case mPages of 
        Just b -> return $ Map.elems (snd  b)
        Nothing -> return []

getBrowserByPage :: BrowserManager -> Page -> IO (Maybe (BrowserId,GladeBrowser))
getBrowserByPage (BM bm) page = do
    withMVar bm (return . Map.foldWithKey findBrowser Nothing)
 where findBrowser _ _ x@(Just _) = x
       findBrowser bid (browser,pMap) Nothing =
            let page' = Map.filter ((== page).snd) pMap
            in if Map.null page'
             then Nothing
             else Just (bid,browser)

-- TODO unevil
getContainerForPage :: BrowserManager -> BrowserId -> Page -> IO (Maybe Container)
getContainerForPage (BM bm) bid page = 
    withMVar bm (return . selectContainer . Map.lookup bid)
  where selectContainer Nothing      = Nothing
        selectContainer (Just (_,m)) = Map.foldWithKey 
            (\ k (c,page') s -> case s of 
                o@(Just _) -> o
                Nothing -> if page == page' then (Just c) else Nothing ) Nothing m
