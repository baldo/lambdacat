{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, RankNTypes, TemplateHaskell #-}

module LambdaCat.UI.Glade where

import LambdaCat.Class
import LambdaCat.Configure (lambdaCatConf, LambdaCatConf (..))

import LambdaCat.UI.Glade.PersistentTabId
import LambdaCat.Session
import LambdaCat.Supply 

import Paths_lambdacat

import Data.Maybe
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Network.URI


data GladeUI = GladeUI
   { gladeXML      :: GladeXML
   , gladeWindow   :: Window
   , gladeStatBar  :: Statusbar
   , viewContainer :: Notebook
   , gladeSession  :: Session TabId TabMeta 
   }

data TabMeta = TabMeta 
  { tabMetaIdent :: TabId
  , tabMetaLabel :: Label
  , tabMetaImage :: Image 
  }

instance UIClass GladeUI TabMeta where
  init = do
      _ <- initGUI

      spath    <- getDataFileName "lambdacat.gtkrc"
      rcParse spath

      fpath    <- getDataFileName "lambdacat.glade"
      Just xml <- xmlNew fpath
      window   <- xmlGetWidget xml castToWindow "mainWindow"
      notebook <- xmlGetWidget xml castToNotebook "viewNotebook"
      statbar  <- xmlGetWidget xml castToStatusbar "statusbar"
      return GladeUI { gladeSession = newSession
                     , gladeXML     = xml
                     , gladeWindow  = window 
                     , gladeStatBar = statbar
                     , viewContainer= notebook 
                     }

  mainLoop ui = do
      let notebook = viewContainer ui
          statbar  = gladeStatBar ui
          xml      = gladeXML ui
          window   = gladeWindow ui
          session  = gladeSession ui
      -- General / Events ---------------------------------------------------
      _ <- onDestroy window mainQuit
      _ <- notebook `on`  switchPage $ \ newActive -> do
        withNthNotebookTab notebook session newActive $ \ tab -> do
            changedURI   (tabView tab) ui (tabMeta tab)
            changedTitle (tabView tab) ui (tabMeta tab)
     -- Toolbar / Events ---------------------------------------------------
      addTab <- xmlGetToolButton xml "addTabButton"
      let Just defaultURI = parseURI "about:blank"
      _ <- onToolButtonClicked addTab $ supplyForView (update ui (undefined :: TabMeta)) embedView defaultURI

      homeButton <- xmlGetToolButton xml "homeButton"
      _ <- onToolButtonClicked homeButton $ supplyForView (update ui (undefined :: TabMeta)) replaceView $ homeURI lambdaCatConf

      quitItem <- xmlGetWidget xml castToMenuItem "quitItem"
      _ <- onActivateLeaf quitItem mainQuit

      let Just infoURI = parseURI "about:info"
      infoItem <- xmlGetWidget xml castToMenuItem "infoItem"
      _ <- onActivateLeaf infoItem $ supplyForView (update ui (undefined :: TabMeta)) embedView infoURI

      {- Review following code
      pageBack <- xmlGetToolButton xml "pageBack"
      _ <- onToolButtonClicked pageBack (pageAction notebook bid (\_ p -> back p))
      pageForward <- xmlGetToolButton xml "pageForward"
      _ <- onToolButtonClicked pageForward (pageAction notebook bid (\_ p -> forward p))
      pageReload <- xmlGetToolButton xml "pageReload"
      _ <- onToolButtonClicked pageReload (pageAction notebook bid (\_ p -> reload p))
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
      -}
      widgetShowAll window
      -- start GTK mainloop
      mainGUI
     {-

     where

        loadAction :: URI -> BrowserId -> TabId -> View -> IO ()
        loadAction uri bid tid w = do
            mw' <- pageFromProtocol (update ui bid)
                                    (uriModifier lambdaCatConf)
                                    (pageList lambdaCatConf)
                                    (Just w)
                                    (Just uri)
            case mw' of
                -- TODO call an default error page
                Nothing -> return ()
                Just (w', uri') -> do
                    replacePage ui bid tid w w'
                    _ <- load w' uri'
                    return ()

        pageAction :: Notebook -> BrowserId -> (TabId -> View -> IO a) -> IO ()
        pageAction notebook bid f = do
            -- TODO select correct page
            tid <- notebookGetCurrentPage notebook
            mcontainer <- notebookGetNthPage notebook tid
            case mcontainer of
                Just container ->
                    withContainerId (castToContainer container) $ \ tabId -> do
                        mPage <- getPageFromBrowser (browsers ui) bid tabId
                        case mPage of
                            Just (_, _, p) -> f tabId p >> return ()
                            Nothing -> return ()
                Nothing -> do
                    mp <- newPage bid $ parseURI "about:blank"
                    case mp of
                        Just p -> do
                            -- _ <- f undefined p 
                            return ()
                        Nothing ->
                            return ()

    -}

  update ui meta f = f ui meta

  changedURI view ui meta = do
      let xml = gladeXML ui 
      pageURI <- xmlGetWidget xml castToEntry "pageURI"
      uri <- getCurrentURI view
      entrySetText pageURI (uriString uri)
    where uriString uri = uriToString id uri ""

  changedTitle view ui meta = do
      let xml   = gladeXML ui
          tab = getTab (gladeSession ui) (tabMetaIdent meta)
          label = tabMetaLabel meta
      title <- getCurrentTitle view
      set label [ labelLabel := if null title then "(Untitled)" else title ]
      return ()
      window <- xmlGetWidget xml castToWindow "browserWindow"
      set window [ windowTitle := title ]

  changedProgress progress ui meta = do 
      let sb = gladeStatBar ui
      -- TODO check if current tab is equal to the tab which hosts the calling
      -- view 
      cntx <- statusbarGetContextId sb "progress"
      statusbarPop sb cntx
      _ <- statusbarPush sb cntx $ show progress ++ "%"
      return ()

  changedStatus status ui meta = do
      let sb = gladeStatBar ui 
      -- TODO check if current tab is equal to the tab which hosts the 
      -- calling view
      cntx <- statusbarGetContextId sb "status"
      case status of
        ""   -> statusbarPop sb cntx
        stat -> do
          statusbarPop sb cntx
          _ <- statusbarPush sb cntx stat
          return ()

  replaceView view ui meta = do
      let Just tab   = getTab (gladeSession ui) (tabMetaIdent meta) 
          oldView    = tabView tab
          newSession = updateTab (gladeSession ui) (tabMetaIdent meta) $ \ t -> Just $ t { tabView = view } 
          container  = viewContainer ui 
      destroy oldView
      mapM_ (containerRemove container) =<< containerGetChildren container
      embed view (\w -> containerAdd container w >> widgetShowAll w) (update ui meta)
      return ()

  embedView view ui _ = do
    let noteBook = viewContainer ui
        session  = gladeSession ui 
    scrolledWindow <- scrolledWindowNew Nothing Nothing  
    tabId          <- genNewId
    setContainerId scrolledWindow tabId 
    (labelWidget, img, label) <- tabWidget (do
              removeTId <- get noteBook (notebookChildPosition scrolledWindow)
              notebookRemovePage noteBook removeTId
              withContainerId scrolledWindow $ \ removeTabId ->
                    destroy $ tabView . fromJust $ getTab session removeTabId
              )
    let tabMeta = TabMeta 
          { tabMetaIdent = tabId 
          , tabMetaLabel = label
          , tabMetaImage = img 
          }
    embed view (embedHandle scrolledWindow) (update ui (undefined :: TabMeta))
    notebookAppendPageMenu noteBook scrolledWindow labelWidget labelWidget
    widgetShowAll noteBook
    return ()
   where embedHandle scrolledWindow widget = do
          containerAdd scrolledWindow widget
          return () 
         tabWidget closeCallback = do
          hbox  <- hBoxNew False 3
          label <- labelNew (Just "(Untitled)")
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
          return (hbox, img, label)



xmlGetToolButton :: GladeXML -> String -> IO ToolButton
xmlGetToolButton xml = xmlGetWidget xml castToToolButton

withNthNotebookTab :: Notebook -> Session TabId TabMeta -> Int
                   -> (Tab TabMeta -> IO ()) -> IO ()
withNthNotebookTab notebook session page f = do
    mContainer <- notebookGetNthPage notebook page

    case mContainer of
        Just container ->
            withContainerId (castToContainer container) $ \ tabId -> do
                case getTab session tabId of
                    Just tab ->
                        f tab

                    Nothing ->
                        return ()

        Nothing ->
            return ()
