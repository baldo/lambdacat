{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, RankNTypes #-}

module LambdaCat.UI.Glade
    ( GladeUI
    , module LambdaCat.UI
    )
where

import LambdaCat.Configure (lambdaCatConf, LambdaCatConf (..))

import LambdaCat.UI
import LambdaCat.UI.Glade.PersistentTabId
import LambdaCat.Session
import LambdaCat.Supplier

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
   , gladeSession  :: MSession TabId TabMeta 
   }

data TabMeta = TabMeta 
  { tabMetaIdent     :: TabId
  , tabMetaLabel     :: Label
  , tabMetaImage     :: Image 
  , tabMetaContainer :: Container
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
      session <- newMSession
      return GladeUI { gladeSession = session
                     , gladeXML     = xml
                     , gladeWindow  = window 
                     , gladeStatBar = statbar
                     , viewContainer= notebook 
                     }

  mainLoop ui = do
      let notebook = viewContainer ui
          -- statbar  = gladeStatBar ui
          xml      = gladeXML ui
          window   = gladeWindow ui
          session  = gladeSession ui

      tabVisibility notebook

      -- General / Events ---------------------------------------------------
      _ <- onDestroy window mainQuit
      _ <- notebook `on`  switchPage $ \ newActive -> 
        withNthNotebookTab notebook session newActive $ \ tab -> do
            changedURI   (tabView tab) ui (tabMeta tab)
            changedTitle (tabView tab) ui (tabMeta tab)
     -- Toolbar / Events ---------------------------------------------------
      addTab <- xmlGetToolButton xml "addTabButton"
      let Just defaultURI = parseURI "about:blank"
      _ <- onToolButtonClicked addTab $ do
          supplyForView (update ui undefined) embedView defaultURI
          tabVisibility notebook

      homeButton <- xmlGetToolButton xml "homeButton"
      _ <- onToolButtonClicked homeButton $ supplyForView (update ui undefined) replaceViewCurrent $ homeURI lambdaCatConf

      addressEntry <- xmlGetWidget xml castToEntry "addressEntry"
      _ <- onEntryActivate addressEntry $ do
          text <- entryGetText addressEntry
          case parseURIReference text of
              Just uri -> 
                  supplyForView (update ui $ error "addressEntry") replaceViewCurrent uri
              Nothing ->
                  return ()

      addressItem <- xmlGetWidget xml castToToolItem "addressItem"
      addressItem `set` [ toolItemExpand := True ]

      quitItem <- xmlGetWidget xml castToMenuItem "quitItem"
      _ <- onActivateLeaf quitItem mainQuit

      let Just infoURI = parseURI "about:info"
      infoItem <- xmlGetWidget xml castToMenuItem "infoItem"
      _ <- onActivateLeaf infoItem $ supplyForView (update ui undefined) embedView infoURI

      {- Review following code
      pageBack <- xmlGetToolButton xml "pageBack"
      _ <- onToolButtonClicked pageBack (pageAction notebook bid (\_ p -> back p))
      pageForward <- xmlGetToolButton xml "pageForward"
      _ <- onToolButtonClicked pageForward (pageAction notebook bid (\_ p -> forward p))
      -}
      pageReload <- xmlGetToolButton xml "reloadButton"
      _ <- onToolButtonClicked pageReload $ do
        pageId <- notebookGetCurrentPage notebook
        withNthNotebookTab notebook session pageId $ \ tab ->
          let view = tabView tab
          in  getCurrentURI view >>= load view >> return ()
      {-
      pageHome <- xmlGetToolButton xml "pageHome"
      _ <- onToolButtonClicked pageHome $
              pageAction notebook bid $
                  loadAction (homeURI lambdaCatConf) bid
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

  changedURI view ui _meta = do
      let xml = gladeXML ui 
      pageURI <- xmlGetWidget xml castToEntry "addressEntry"
      uri <- getCurrentURI view
      entrySetText pageURI (uriString uri)
    where uriString uri = uriToString id uri ""

  changedTitle view ui meta = do
      let xml   = gladeXML ui
          label = tabMetaLabel meta
      title <- getCurrentTitle view
      set label [ labelLabel := if null title then "(Untitled)" else title ]
      window <- xmlGetWidget xml castToWindow "mainWindow"
      set window [ windowTitle := title ]
      return ()

  changedProgress progress ui _meta = do 
      let sb = gladeStatBar ui
      -- TODO check if current tab is equal to the tab which hosts the calling
      -- view 
      cntx <- statusbarGetContextId sb "progress"
      statusbarPop sb cntx
      _ <- statusbarPush sb cntx $ show progress ++ "%"
      return ()

  changedStatus status ui _meta = do
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
    replaceViewLocal view (tabMetaContainer meta) ui meta 
    putStrLn "replaceView"
    updateMSession (gladeSession ui) $ \ session ->
      let Just tab   = getTab (tabMetaIdent meta) session
          oldView    = tabView tab
          session'   = updateTab session (tabMetaIdent meta) $ \ t -> Just $ t { tabView = view } 
      in  do 
          destroy oldView
          return (session', ())

  embedView view ui _ = do
    let noteBook = viewContainer ui
    scrolledWindow <- scrolledWindowNew Nothing Nothing  
    tabId          <- genNewId
    setContainerId scrolledWindow tabId 
    (labelWidget, img, label) <- tabWidget (do 
              removeTId <- get noteBook (notebookChildPosition scrolledWindow)
              notebookRemovePage noteBook removeTId
              putStrLn "removeHdl"
              withMSession (gladeSession ui) $ \ session -> 
                withContainerId scrolledWindow $ \ removeTabId ->
                    destroy . tabView . fromJust $ getTab removeTabId session
              tabVisibility noteBook
              )
    let newMeta = TabMeta 
          { tabMetaIdent = tabId 
          , tabMetaLabel = label
          , tabMetaImage = img 
          , tabMetaContainer = castToContainer scrolledWindow
          }
    embed view (embedHandle scrolledWindow) (update ui newMeta)
    putStrLn "embedView"
    updateMSession (gladeSession ui) $ \ session ->
        return (newTab tabId view newMeta nullURI session,())
    _ <- notebookAppendPageMenu noteBook scrolledWindow labelWidget labelWidget
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

tabVisibility :: Notebook -> IO ()
tabVisibility notebook = do
    pages <- notebookGetNPages notebook
    set notebook [ notebookShowTabs := pages > 1 ]

xmlGetToolButton :: GladeXML -> String -> IO ToolButton
xmlGetToolButton xml = xmlGetWidget xml castToToolButton

withNthNotebookTab :: Notebook -> MSession TabId TabMeta -> Int
                   -> (Tab TabMeta -> IO ()) -> IO ()
withNthNotebookTab notebook msession page f = do
    mContainer <- notebookGetNthPage notebook page
    case mContainer of
        Just container -> 
            withContainerId (castToContainer container) $ \ tabId -> do
              putStrLn "withNthNotebookTab"
              withMSession msession $ \ session  -> 
                case getTab tabId session of
                    Just tab -> 
                        f tab
                    Nothing -> 
                        return ()
        Nothing ->
            return ()

replaceViewLocal :: View -> Container -> GladeUI -> TabMeta -> IO ()
replaceViewLocal view container ui meta = do
  mapM_ (containerRemove container) =<< containerGetChildren container
  embed view (\w -> containerAdd container w >> widgetShowAll w) (update ui meta)
  title <- getCurrentTitle view
  set (tabMetaLabel meta) [ labelLabel := if null title then "(Untitled)" else title ]
    
replaceViewCurrent :: View -> GladeUI -> a -> IO ()
replaceViewCurrent view ui _ = do
  let notebook = viewContainer ui
  pageId <- notebookGetCurrentPage notebook
  mContainer <- notebookGetNthPage notebook pageId
  case mContainer of
    Just container -> 
      withContainerId (castToContainer container) $ \ tabId -> do
      putStrLn "replaceViewCurrent"
      meta <- updateMSession (gladeSession ui) $ \ session  -> 
         case getTab tabId session of
          Just tab -> do
            let oldView    = tabView tab
                session'   = updateTab session (tabMetaIdent$ tabMeta tab) $ \ t -> Just $ t { tabView = view } 
            destroy oldView
            return (session', tabMeta tab)
          Nothing ->
            return (session,error "there is no current tab")
      replaceViewLocal view (castToContainer container) ui meta
    Nothing -> return ()
