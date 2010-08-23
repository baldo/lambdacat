{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, RankNTypes, TemplateHaskell #-}

module LambdaCat.UI.Glade where

import LambdaCat.Browser
import LambdaCat.Page
import LambdaCat.Configure (lambdaCatConf, LambdaCatConf (..))

import LambdaCat.Utils
import LambdaCat.UI.Glade.PersistentTabId
import LambdaCat.UI.Glade.BrowserManager

import Paths_lambdacat

import Data.Maybe
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Network.URI


data GladeUI = GladeUI
   { browsers  :: BrowserManager
   }

instance UIClass GladeUI where
    init = do
     _ <- initGUI
     bm <- newBrowserManager
     return GladeUI { browsers = bm }

    newBrowser ui = do
        spath    <- getDataFileName "lambdacat.gtkrc"
        rcParse spath

        fpath    <- getDataFileName "lambdacat.glade"
        Just xml <- xmlNew fpath
        window   <- xmlGetWidget xml castToWindow "browserWindow"
        notebook <- xmlGetWidget xml castToNotebook "pageNoteBook"

        let browser = GladeBrowser { gladeXml = xml, gladeWindow = window, pageContainer = notebook }
        bid <- addBrowser (browsers ui) browser
        -- General / Events ---------------------------------------------------
        _ <- onDestroy window mainQuit
        _ <- notebook `on`  switchPage $ \ newActive -> do
            -- we assert that there is a container in a tab
            (Just container) <- notebookGetNthPage notebook newActive
            withContainerId (castToContainer container) $ \ tabId -> do
                mPage <- getPageFromBrowser (browsers ui) bid tabId
                case mPage of
                    Nothing  -> return ()
                    Just (_, p) -> do
                        uriChanged ui p
                        changedTitle ui p

        -- Toolbar / Events ---------------------------------------------------
        pageBack <- xmlGetToolButton xml "pageBack"
        _ <- onToolButtonClicked pageBack (pageAction notebook bid back)
        pageForward <- xmlGetToolButton xml "pageForward"
        _ <- onToolButtonClicked pageForward (pageAction notebook bid forward)
        pageReload <- xmlGetToolButton xml "pageReload"
        _ <- onToolButtonClicked pageReload (pageAction notebook bid reload)
        pageHome <- xmlGetToolButton xml "pageHome"
        _ <- onToolButtonClicked pageHome $
                pageAction notebook bid $
                    loadAction (homeURI lambdaCatConf) bid
        pageURI <- xmlGetWidget xml castToEntry "pageURI"
        _ <- onEntryActivate pageURI  $ do
            text <- entryGetText pageURI
            case parseURIReference text of
                Just uri -> pageAction notebook bid $ loadAction uri bid
                Nothing  -> return ()
        addTab <- xmlGetToolButton xml "addTab"
        _ <- onToolButtonClicked addTab $ newPage bid (parseURI "about:blank") >> return ()
        menuItemQuit <- xmlGetWidget xml castToMenuItem "menuItemQuit"
        _ <- onActivateLeaf menuItemQuit mainQuit
        menuItemInfo <- xmlGetWidget xml castToMenuItem "menuItemInfo"
        _ <- onActivateLeaf menuItemInfo $ newPage bid (parseURI "about:info") >> return ()
        widgetShowAll window
        return bid

     where
        newPage :: BrowserId -> Maybe URI -> IO (Maybe Page)
        newPage bid uri = do
            mw <- pageFromProtocol (update ui bid)
                                   (uriModifier lambdaCatConf)
                                   (pageList lambdaCatConf)
                                   Nothing
                                   uri
            case mw of
                -- TODO call an default error page
                Nothing -> return Nothing
                Just (w, uri') -> do
                    embedPage ui bid w
                    _ <- load w uri'
                    return $ Just w

        loadAction :: URI -> BrowserId -> Page -> IO ()
        loadAction uri bid w = do
            mw' <- pageFromProtocol (update ui bid)
                                    (uriModifier lambdaCatConf)
                                    (pageList lambdaCatConf)
                                    (Just w)
                                    (Just uri)
            case mw' of
                -- TODO call an default error page
                Nothing -> return ()
                Just (w', uri') -> do
                    replacePage ui bid w w'
                    _ <- load w' uri'
                    return ()

        pageAction :: Notebook -> BrowserId -> (Page -> IO a) -> IO ()
        pageAction notebook bid f = do
            -- TODO select correct page
            tid <- notebookGetCurrentPage notebook
            mcontainer <- notebookGetNthPage notebook tid
            case mcontainer of
                Just container -> do
                    withContainerId (castToContainer container) $ \ tabId -> do
                        mPage <- getPageFromBrowser (browsers ui) bid tabId
                        case mPage of
                            Just (_, p) -> f p >> return ()
                            Nothing -> return ()
                Nothing -> do
                    mp <- newPage bid $ parseURI "about:blank"
                    case mp of
                        Just p -> do
                            _ <- f p
                            return ()
                        Nothing ->
                            return ()

        xmlGetToolButton :: GladeXML -> String -> IO ToolButton
        xmlGetToolButton xml name = xmlGetWidget xml castToToolButton name

    update ui bid f = do
        f ui bid
        return ()

    uriChanged ui page = do
        p <- getBrowserByPage (browsers ui) page
        case p of
          Just (_, GladeBrowser { gladeXml = xml }) -> do
            pageURI <- xmlGetWidget xml castToEntry "pageURI"
            uri <- getCurrentURI page
            entrySetText pageURI (uriString uri)
          Nothing  -> return ()
      where uriString uri = uriToString id uri ""

    changedTitle ui page = do
        mBrowser <- getBrowserByPage (browsers ui) page
        case mBrowser of
            Nothing -> return ()
            Just (bid, GladeBrowser { gladeXml = xml}) -> do
                title <- getCurrentTitle page
                mContainer  <- getContainerForPage (browsers ui) bid page
                case mContainer of
                    Nothing  -> return ()
                    Just _container -> do
                        _notebook <- xmlGetWidget xml castToNotebook "pageNoteBook"
                        {- mTab <- notebookGetNthPage notebook tid
                        case mTab of
                            (Just tab) -> return () --notebookSetTabLabelText notebook tab title
                            Nothing    -> return ()
                        -}
                        -- TODO unyeha
                        return ()
                window <- xmlGetWidget xml castToWindow "browserWindow"
                set window [ windowTitle := title ]

    replacePage ui bid oldpage page@(Page hasWidget) = do
        bool <- getBrowser (browsers ui) bid
        $plog putStrLn ("replacePage: " ++ show bool)
        case bool of
          Just (GladeBrowser { gladeXml = xml }) -> do
            let widget = getWidget hasWidget
            _noteBook <- xmlGetWidget xml castToNotebook "pageNoteBook"
            -- Replace page in container
            $plog putStrLn ("replacePage bid: " ++ show bid)
            maybeContainer <- getContainerForPage (browsers ui) bid oldpage
            $plog putStrLn ("replacePage container: " ++ show (isJust maybeContainer))
            case maybeContainer of
              Just container -> do
                mapM_ (containerRemove container) =<< containerGetChildren container
                containerAdd container widget
                widgetShowAll widget
                -- Replace page in state/model
                replacePageInBrowser (browsers ui) bid oldpage page
                destroy oldpage
                return ()
              Nothing -> return ()
          Nothing -> return ()

    embedPage ui bid page@(Page hasWidget) = do
        bool <- getBrowser (browsers ui) bid
        $plog putStrLn ("embedPage: " ++ show bool)
        case bool of
          Just (GladeBrowser { gladeXml = xml }) -> do
            let widget = getWidget hasWidget
            noteBook  <- xmlGetWidget xml castToNotebook "pageNoteBook"
            scrolledWindow <- scrolledWindowNew Nothing Nothing
            containerAdd scrolledWindow widget

            tabId <- genNewId
            setContainerId scrolledWindow tabId

            _tabId <- notebookAppendPage noteBook scrolledWindow "(No Title)"
            labelWidget <- tabWidget (do
                removeTId <- get noteBook (notebookChildPosition scrolledWindow)
                notebookRemovePage noteBook removeTId
                withContainerId scrolledWindow $ \ removeTabId -> do
                    -- we assume that any existing tab should have a page in it.
                    Just (_, removePage) <- getPageFromBrowser (browsers ui) bid removeTabId
                    removePageFromBrowser (browsers ui) bid removePage
                    destroy removePage
                )
            notebookSetTabLabel noteBook scrolledWindow labelWidget
            widgetShowAll noteBook
            addPageToBrowser (browsers ui) bid tabId (castToContainer scrolledWindow) page
            return ()
          Nothing -> return ()
      where tabWidget closeCallback = do
                hbox  <- hBoxNew False 3
                label <- labelNew (Just "YEHA")

                button <- buttonNew
                widgetSetName button "tab-close-button"

                fav <- imageNewFromStock stockJustifyCenter IconSizeMenu

                img <- imageNewFromStock stockClose IconSizeMenu

                set button
                    [ buttonRelief := ReliefNone
                    , buttonImage  := img
                    ]

                _ <- button `onClicked` closeCallback

                boxPackStart hbox fav PackGrow 0
                boxPackStart hbox label PackGrow 0
                boxPackStart hbox button PackNatural 0

                widgetShowAll hbox
                return hbox

    mainLoop _ = mainGUI
