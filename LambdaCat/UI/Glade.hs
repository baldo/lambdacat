{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, OverloadedStrings, TypeSynonymInstances, RankNTypes #-}

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
import LambdaCat.History 
import LambdaCat.Utils

import Paths_lambdacat

import Data.Maybe
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Network.URI
import Control.Monad (when)
import Control.Monad.Trans

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

  mainLoop ui =
      let notebook = viewContainer ui
          -- statbar  = gladeStatBar ui
          xml      = gladeXML ui
          window   = gladeWindow ui
          session  = gladeSession ui
      in  do 
      tabVisibility notebook

      -- General / Events ---------------------------------------------------
      _ <- onDestroy window mainQuit
      _ <- notebook `on`  switchPage $ \ newActive -> do 
        (view,meta) <- withNthNotebookTab notebook session newActive $ 
          \ tab -> return (tabView tab,tabMeta tab)
        uri <- getCurrentURI view
        progress <- getCurrentProgress view
        updateAddressBar ui uri
        updateProgress   ui progress
        changedTitle view ui meta
      -- Toolbar / Events ---------------------------------------------------
      addressEntry <- xmlGetWidget xml castToEntry "addressEntry"

      addTab <- xmlGetToolButton xml "addTabButton"
      _ <- onToolButtonClicked addTab $ do 
          supplyForView (update ui undefined) embedView $ defaultURI lambdaCatConf
          widgetGrabFocus addressEntry 

      homeButton <- xmlGetToolButton xml "homeButton"
      _ <- onToolButtonClicked homeButton $ supplyForView (update ui undefined) replaceViewCurrent $ homeURI lambdaCatConf

      _ <- addressEntry `on` keyPressEvent $ do
          val <- eventKeyVal
          case keyName val of
              "Return" -> do
                  text <- liftIO $ entryGetText addressEntry
                  case stringToURI text of
                      uri
                        | uri /= nullURI -> do
                          liftIO $ supplyForView (update ui $ error "addressEntry") replaceViewCurrent uri
                          return True
                        | otherwise ->
                          return False -- handle error

              _ ->
                return False

      addressItem <- xmlGetWidget xml castToToolItem "addressItem"
      addressItem `set` [ toolItemExpand := True ]
 
      quitItem <- xmlGetWidget xml castToMenuItem "quitItem"
      _ <- onActivateLeaf quitItem mainQuit

      let infoURI = "about:info"
      infoItem <- xmlGetWidget xml castToMenuItem "infoItem"
      _ <- onActivateLeaf infoItem $ supplyForView (update ui undefined) embedView infoURI

      pageBack <- xmlGetToolButton xml "backButton"
      _ <- onToolButtonClicked pageBack $ do
        (view,muri) <- withCurrentTab ui $ \ tab tabId sess -> 
         let history  = tabHistory tab
             history' = if hasBack history
                        then fromJust $ back history
                        else history
             newuri   = current history'
             view     = tabView tab
         in  return (updateTab sess tabId $ const .  Just $ tab { tabHistory = history' }
                    ,(view,if hasBack history then Just newuri else Nothing))
        maybe (return ()) (\uri -> load view uri >> return ()) muri

      forwardButton <- xmlGetToolButton xml "forwardButton"
      _ <- onToolButtonClicked forwardButton $ do
        (view,muri) <- withCurrentTab ui $ \ tab tabId sess -> 
         let history  = tabHistory tab
             history' = if hasForward history
                        then fromJust $ forward (fst . last . getForwards $ history ) history 
                        else history
             newuri   = current history'
             view     = tabView tab
         in  return (updateTab sess tabId $ const .  Just $ tab { tabHistory = history' }
                    ,(view,if hasForward history then Just newuri else Nothing))
        maybe (return ()) (\uri -> load view uri >> return ()) muri


      pageReload <- xmlGetToolButton xml "reloadButton"
      _ <- onToolButtonClicked pageReload $ do 
        view <- withCurrentTab ui $ \ tab _ sess -> 
          return (sess,tabView tab)
        _ <- getCurrentURI view >>= load view
        return ()


      widgetShowAll window
      -- start GTK mainloop
      mainGUI

  changedURI view ui meta =
     let ident = tabMetaIdent meta 
         thisTabId = tabMetaIdent meta
     in  do uri <- getCurrentURI view
            doit <- withCurrentTab ui $ \ _ tabid session -> 
              return (session,tabid == thisTabId)
            when doit $ updateAddressBar ui uri 
            updateMSession (gladeSession ui) $ \ session -> return
              (updateTab session ident $ \tab -> 
                let history = tabHistory tab 
                in Just $ tab { tabHistory = updateCurrent uri history }
              ,())
            return ()

  changedTitle view ui meta = do
      let label  = tabMetaLabel meta
          window = gladeWindow ui
      title <- getCurrentTitle view
      set label [ labelLabel := if null title then defaultTitle lambdaCatConf else title ]
      set window [ windowTitle := title ]
      return ()

  changedProgress progress ui meta = do 
      let thisTabId = tabMetaIdent meta
      doit <- withCurrentTab ui $ \ _ tabid session -> 
        return (session,tabid == thisTabId)
      when doit $ updateProgress ui progress

  changedStatus status ui meta = do
      let sb = gladeStatBar ui 
          thisTabId = tabMetaIdent meta
      doit <- withCurrentTab ui $ \ _ tabid session -> 
        return (session,tabid == thisTabId)
      when doit $ do 
        cntx <- statusbarGetContextId sb "status"
        case status of
          ""   -> statusbarPop sb cntx
          stat -> do
            statusbarPop sb cntx
            _ <- statusbarPush sb cntx stat
            return ()

  replaceView view ui meta = do 
    replaceViewLocal view (tabMetaContainer meta) ui meta 
    newURI <- getCurrentURI view 
    oldView <- updateMSession (gladeSession ui) $ \ session -> 
      let Just tab   = getTab (tabMetaIdent meta) session
          oldView    = tabView tab
          history    = tabHistory tab
          history'   = insertAndForward newURI history 
          session'   = updateTab session (tabMetaIdent meta) $ \ t -> Just $ t { tabView = view, tabHistory = history' } 
      in return (session', oldView)
    destroy oldView

  embedView view ui _ = do
    let noteBook = viewContainer ui
    scrolledWindow <- scrolledWindowNew Nothing Nothing  
    tabId          <- genNewId
    setContainerId scrolledWindow tabId 
    (labelWidget, img, label) <- tabWidget (do 
              removeTId <- get noteBook (notebookChildPosition scrolledWindow)
              notebookRemovePage noteBook removeTId
              withContainerId scrolledWindow $ \ removeTabId -> do
                kView <- updateMSession (gladeSession ui) $ \ session -> 
                  let kv = tabView . fromJust $ getTab removeTabId session
                  in return (deleteTab removeTabId session,kv)
                destroy kView
              tabVisibility noteBook
              )
    let newMeta = TabMeta 
          { tabMetaIdent = tabId 
          , tabMetaLabel = label
          , tabMetaImage = img 
          , tabMetaContainer = castToContainer scrolledWindow
          }
    embed view (embedHandle scrolledWindow) (update ui newMeta)
    startURI <- getCurrentURI view 
    updateMSession (gladeSession ui) $ \ session ->
        let session' = newTab tabId view newMeta startURI session
        in  return (session' {sessionTabActive = Just tabId},())
    pageId <- notebookAppendPageMenu noteBook scrolledWindow labelWidget labelWidget
    widgetShowAll noteBook
    tabVisibility noteBook
    notebookSetCurrentPage noteBook pageId
    return ()
   where embedHandle scrolledWindow widget = do
          containerAdd scrolledWindow widget
          return () 
         tabWidget closeCallback = do
          hbox  <- hBoxNew False 3
          label <- labelNew (Just $ defaultTitle lambdaCatConf)
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
                   -> (Tab TabMeta -> IO a) -> IO a
withNthNotebookTab notebook msession page f = do
    mContainer <- notebookGetNthPage notebook page
    case mContainer of
        Just container -> 
            withUnsafeContainerId (castToContainer container) $ \ tabId -> 
              withMSession msession $ \ session -> 
                case getTab tabId session of
                    Just tab -> f tab
                    Nothing -> error "no tab with such an id"
        Nothing -> error "no container in here"

withCurrentTab :: GladeUI
               -> (Tab TabMeta -> TabId -> Session TabId TabMeta -> IO (Session TabId TabMeta,a))
               -> IO a 
withCurrentTab ui f =
  let notebook = viewContainer ui
      msession = gladeSession ui
  in  do 
    pageId <- notebookGetCurrentPage notebook 
    mContainer <- notebookGetNthPage notebook pageId 
    case mContainer of
      Just container -> 
         withUnsafeContainerId (castToContainer container) $ \tabId -> 
          updateMSession msession $ \ session -> 
            case getTab tabId session of
              Just tab -> f tab tabId session 
              Nothing  -> error "Can't find current tab"
      Nothing -> error "there is no tab with the given ident in the notebook"

replaceViewLocal :: View -> Container -> GladeUI -> TabMeta -> IO ()
replaceViewLocal view container ui meta = do
  mapM_ (containerRemove container) =<< containerGetChildren container
  embed view (\w -> containerAdd container w >> widgetShowAll w >> widgetGrabFocus w) (update ui meta)
  title <- getCurrentTitle view
  set (tabMetaLabel meta) [ labelLabel := if null title then defaultTitle lambdaCatConf else title ]

    
replaceViewCurrent :: View -> GladeUI -> a -> IO ()
replaceViewCurrent view ui _ = do
  let notebook = viewContainer ui
  pageId <- notebookGetCurrentPage notebook
  mContainer <- notebookGetNthPage notebook pageId
  case mContainer of
    Just container -> do 
      newURI <- getCurrentURI view 
      (meta,oldView) <- withUnsafeContainerId (castToContainer container) $ \ tabId -> do
        updateMSession (gladeSession ui) $ \ session  ->
          case getTab tabId session of
          Just tab -> do
            let oldView    = tabView tab
                history    = tabHistory tab
                history'   = insertAndForward newURI history 
                session'   = updateTab session (tabMetaIdent $ tabMeta tab) $ \ t -> Just $ t { tabView = view, tabHistory = history' }
            return (session' {sessionTabActive = Just tabId},(tabMeta tab,oldView))
          Nothing -> 
            return (session,error "there is no current tab")
      destroy oldView
      replaceViewLocal view (castToContainer container) ui meta
    Nothing -> return ()

updateAddressBar :: GladeUI -> URI -> IO ()
updateAddressBar ui uri = 
  let xml = gladeXML ui 
  in  do pageURI <- xmlGetWidget xml castToEntry "addressEntry"
         entrySetText pageURI (uriToString id uri "")

updateProgress :: GladeUI -> Int -> IO () 
updateProgress ui progress = do 
  let sb = gladeStatBar ui
  cntx <- statusbarGetContextId sb "progress"
  statusbarPop sb cntx
  _ <- statusbarPush sb cntx $ 
    if progress < 100 
       then show progress ++ "%"
       else "Done" 
  return ()

