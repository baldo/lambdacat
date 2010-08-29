{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
    , getLabelAndContainerForPage
    ) where

import LambdaCat.Browser (BrowserId, newBrowserId)
import LambdaCat.Page
import LambdaCat.Utils
import LambdaCat.UI.Glade.PersistentTabId (TabId)

import Control.Concurrent.MVar
import Data.Map (Map)
import qualified Data.Map as Map
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

data GladeBrowser = GladeBrowser
    { gladeXml      :: GladeXML
    , gladeWindow   :: Window
    , gladeStatBar  :: Statusbar
    , pageContainer :: Notebook
    }

instance Show GladeBrowser where
    show _ = "GladeBrowser"
newtype BrowserManager = BM (MVar BrowserMap)

type BrowserMap = Map BrowserId (GladeBrowser, TabMap)
type TabMap = Map TabId (TabLabel, Container, Page)
type TabLabel = (Image, Label)

instance Show Container where
    show _ = "Container"

instance Show Image where
    show _ = "Image"

instance Show Label where
    show _ = "Label"

thrd :: (a, b, c) -> c
thrd (_, _, c) = c

newBrowserManager :: IO BrowserManager
newBrowserManager = do
    m <- newMVar Map.empty
    return (BM m)

addBrowser :: BrowserManager -> GladeBrowser -> IO BrowserId
addBrowser (BM bm) g = do
    bid <- newBrowserId
    modifyMVar_ bm (return . Map.insert bid (g, Map.empty))
    return bid

{- | Add a (container, page) to the browser identified by BrowserId, if there
     is already a page with given TabId then the page gets replaced.
-}
addPageToBrowser :: BrowserManager -> BrowserId -> TabId -> Image -> Label -> Container -> Page -> IO ()
addPageToBrowser (BM bm) bid tid img lbl container page =
  modifyMVar_ bm (return . Map.update (\ (bw, pages) -> Just (bw, Map.insert tid ((img, lbl), container, page) pages)) bid)

removePageFromBrowser :: BrowserManager -> BrowserId -> Page -> IO ()
removePageFromBrowser (BM bm) bid page =
  modifyMVar_ bm (return . Map.update (\ (bw, pages) -> Just (bw, Map.filter ((/= page) . thrd) pages)) bid)

replacePageInBrowser :: BrowserManager -> BrowserId -> Page -> Page -> IO ()
replacePageInBrowser (BM bm) bid oldpage newpage =
    modifyMVar_ bm (return . Map.update (\ (bw, pages) -> Just (bw, Map.map replace pages)) bid)
  where replace :: (TabLabel, Container, Page) -> (TabLabel, Container, Page)
        replace (tl, container, page) | page == oldpage = (tl, container, newpage)
                                      | otherwise       = (tl, container, page)

countTabsInBrowser :: BrowserManager -> BrowserId -> IO Int
countTabsInBrowser (BM bm) bid =
    withMVar bm (return . size . Map.lookup bid)
  where size (Just (_, m)) = Map.size m
        size Nothing  = 0

{- Don't know if we need this function after refactoring
getTabIdForPage :: BrowserManager -> BrowserId -> Page -> IO (Maybe Int)
getTabIdForPage (BM bm) bid page =
    withMVar bm (return . selectTabId . Map.lookup bid)
  where selectTabId Nothing      = Nothing
        selectTabId (Just (_, m)) = Map.foldWithKey
            (\ k page' s -> case s of
                o@(Just _) -> o
                Nothing -> if page == page' then (Just k) else Nothing ) Nothing m
-}

getPageFromBrowser :: BrowserManager -> BrowserId -> TabId -> IO (Maybe (TabLabel, Container, Page))
getPageFromBrowser (BM bm) bid tid =
    withMVar bm (return . getPage . Map.lookup bid)
  where getPage (Just (_, m)) = Map.lookup tid m
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

getBrowserPages :: BrowserManager -> BrowserId -> IO [(TabLabel, Container, Page)]
getBrowserPages (BM bm) bid = do
    -- TODO error handling
    mPages  <- withMVar bm (return . Map.lookup bid)
    case mPages of
        Just b -> return $ Map.elems (snd b)
        Nothing -> return []

getBrowserByPage :: BrowserManager -> Page -> IO (Maybe (BrowserId, GladeBrowser))
getBrowserByPage (BM bm) page =
    withMVar bm (return . Map.foldWithKey findBrowser Nothing)
 where findBrowser _ _ x@(Just _) = x
       findBrowser bid (browser, pMap) Nothing =
            let page' = Map.filter ((== page) . thrd) pMap
            in if Map.null page'
             then Nothing
             else Just (bid, browser)

-- TODO unevil
getLabelAndContainerForPage :: BrowserManager -> BrowserId -> Page -> IO (Maybe (TabLabel, Container))
getLabelAndContainerForPage (BM bm) bid page =
    withMVar bm $ \ bs -> do
        let mb = Map.lookup bid bs
        $plog putStrLn ("getContainerForPage: page = " ++ show page)
        $plog putStrLn ("getContainerForPage: mb   = " ++ show mb)
        return $ selectContainer mb
  where selectContainer Nothing       = Nothing
        selectContainer (Just (_, m)) = Map.foldWithKey
            (\ _k (tl, c, page') s -> case s of
                o@(Just _) -> o
                Nothing -> if page == page' then Just (tl, c) else Nothing) Nothing m
