{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, RankNTypes, TemplateHaskell #-}

module LambdaCat.UI.Glade where

import LambdaCat.Class
import LambdaCat.Configure (lambdaCatConf, LambdaCatConf (..))

import LambdaCat.UI.Glade.PersistentTabId
import LambdaCat.Session
import LambdaCat.Supply 

-- import Paths_lambdacat

import Data.Maybe
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Network.URI


data GladeUI = GladeUI
   { gladeXML      :: GladeXML
   , gladeWindow   :: Window
   , gladeStatBar  :: Statusbar
   , viewContainer :: Notebook
   , gladeSession  :: Session Int TabMeta 
   }

data TabMeta = TabMeta 
  { tabMetaIdent :: Int
  , tabMetaLabel :: Label
  , tabMetaImage :: Image 
  }

instance UIClass GladeUI where
  init = do
      _ <- initGUI

      spath    <-  return undefined -- getDataFileName "lambdacat.gtkrc"
      rcParse spath

      fpath    <- return undefined -- getDataFileName "lambdacat.glade"
      Just xml <- xmlNew fpath
      window   <- xmlGetWidget xml castToWindow "browserWindow"
      notebook <- xmlGetWidget xml castToNotebook "pageNoteBook"
      statbar  <- xmlGetWidget xml castToStatusbar "browserStatus"
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
          --  when this signal is called we assert 
          --  that there is a tab, which contains a container.
          (Just container) <- notebookGetNthPage notebook newActive
          withContainerId (castToContainer container) $ \ tabId -> do
              case getTab session tabId of
                  Nothing  -> return ()
                  Just tab -> do 
                      changeURI   ui tabId (tabView tab)
                      changeTitle ui tabId (tabView tab)

      -- Toolbar / Events ---------------------------------------------------
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
      searchText <- xmlGetWidget xml castToEntry "searchText"
      _ <- onEditableChanged searchText $ do
          text <- entryGetText searchText
          pageAction notebook bid $ (\ _ p -> flip search text  p)
      addTab <- xmlGetToolButton xml "addTab"
      _ <- onToolButtonClicked addTab $ newPage bid (parseURI "about:blank") >> return ()
      menuItemQuit <- xmlGetWidget xml castToMenuItem "menuItemQuit"
      _ <- onActivateLeaf menuItemQuit mainQuit
      menuItemInfo <- xmlGetWidget xml castToMenuItem "menuItemInfo"
      _ <- onActivateLeaf menuItemInfo $ newPage bid (parseURI "about:info") >> return ()
      -}
      widgetShowAll window
      -- start GTK mainloop
      mainGUI
     {-

     where
        newPage :: BrowserId -> Maybe URI -> IO (Maybe View)
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

        xmlGetToolButton :: GladeXML -> String -> IO ToolButton
        xmlGetToolButton xml = xmlGetWidget xml castToToolButton
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
          tab   = getTab (gladeSession ui) (tabMetaIdent meta)
          label = tabMetaLabel tab
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
      -- should we destory old page ?
      let oldView    = tabView $ getTab (gladeSession ui) (tabMetaIdent meta) 
          newSession = updateTab (gladeSession ui) meta $ \ tab -> tab { tabView = view } 
          container  = viewContainer ui 
      destroy oldView
      mapM_ (containerRemove container) =<< containerGetChildren container
      embed view (\w -> containerAdd container w >> widgetShowAll w) 
      return ()

  embedView view ui _ = do
    let noteBook = viewContainer ui
        session  = gladeSession ui 
    scrolledWindow <- scrolledWindowNew Nothing Nothing  
    tabId          <- genNewId
    (labelWidget, img, label) <- tabWidget (do
              removeTId <- get noteBook (notebookChildPosition scrolledWindow)
              notebookRemovePage noteBook removeTId
              withContainerId scrolledWindow $ \ removeTabId ->
                    let tab = getTab session removeTabId
                    in  destroy (tabView tab)
              )
    let tabMeta = TabMeta 
          { tabMetaIdent = tabId 
          , tabMetaLabel = label
          , tabMetaImage = img 
          }
    embed view (embedHandle scrolledWindow) (update ui tabMeta) 
    notebookAppendPage noteBook scrolledWindow labelWidget  
   where embedHandle scrolledWindow widget = do
          containerAdd scrolledWindow widget
          setContainerId scrolledWindow widget
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
        
  {- Can we remove following code now? 

  embedPage ui bid page@(View hasWidget) = do
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
          (labelWidget, img, label) <- tabWidget (do
              removeTId <- get noteBook (notebookChildPosition scrolledWindow)
              notebookRemovePage noteBook removeTId
              withContainerId scrolledWindow $ \ removeTabId -> do
                  -- we assume that any existing tab should have a page in it.
                  Just (_, _, removePage) <- getPageFromBrowser (browsers ui) bid removeTabId
                  removePageFromBrowser (browsers ui) bid removePage
                  destroy removePage
              )
          notebookSetTabLabel noteBook scrolledWindow labelWidge
          widgetShowAll noteBook
          addPageToBrowser (browsers ui) bid tabId img label (castToContainer scrolledWindow) page
          return ()
        Nothing -> return ()

    where tabWidget closeCallback = do
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
  -}
