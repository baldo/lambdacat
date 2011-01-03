{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, RankNTypes #-}

module LambdaCat.UI.Vim
  ( VimUI
  , module LambdaCat.UI
  )
where 

import LambdaCat.Configure (lambdaCatConf, LambdaCatConf (..))

import LambdaCat.UI
import LambdaCat.UI.Glade.PersistentTabId
import LambdaCat.Session
import LambdaCat.Supplier
import LambdaCat.History

import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.WebView (WebView)
import Graphics.UI.Gtk.WebKit.WebView as WebView  
import Control.Concurrent.MVar
import Network.URI
import Control.Monad.Trans

data VimUI = VimUI 
  { vimWindow     :: Window
  , vimVBox       :: VBox
  , vimTabHolder  :: Container 
  , vimMode       :: MVar Mode
  , vimTabCurrent :: MVar View
  , vimTabs       :: MVar [View]
  , vimSession    :: MSession TabId TabMeta 
  , vimStatus     :: WebView
  , vimControl    :: WebView 
  }

data TabMeta = TabMeta
  { tabMetaIdent     :: TabId
  , tabMetaLabel     :: Label
  , tabMetaImage     :: Image 
  , tabMetaContainer :: Container
  }

data Mode = Command | Insert 
  deriving Show 

instance UIClass VimUI TabMeta where 

  init = do
    _ <- initGUI

    window <- windowNew
    vbox   <- vBoxNew False 0
    tabHolder <- hBoxNew False 0
    containerAdd window vbox 
    mode <- newMVar Command 

    -- window settings 
    _ <- onDestroy window mainQuit


    -- status bar 
    status <- webViewNew 
    webViewLoadHtmlString status "<body style=\"color:white;font-weight:bold;font-size:13px;background:black;margin:0;padding:0\">http://www.ccc.de</body>" ""
    widgetSetSizeRequest status (-1) 15 

    -- control / input area 
    control <- webViewNew 
    webViewLoadHtmlString control "<body style=\"color:black;font-weight:bold;font-size:13px;background:white;margin:0;padding:0\"></body>" ""
    widgetSetSizeRequest control (-1) 15 

    -- glue the stuff together 
    boxPackStart vbox tabHolder PackGrow    0 
    boxPackEnd   vbox control   PackNatural 0
    boxPackEnd   vbox status    PackNatural 0

    widgetShowAll window

    current <- newEmptyMVar 
    tabs    <- newMVar [] 
    session <- newMSession

    let ui = VimUI { vimWindow     = window
                  , vimMode       = mode 
                  , vimVBox       = vbox
                  , vimTabHolder  = castToContainer tabHolder
                  , vimTabs       = tabs 
                  , vimTabCurrent = current
                  , vimSession    = session
                  , vimStatus     = status
                  , vimControl    = control 
                  }

    -- simple keyboard handling 
    window `on` keyPressEvent $ tryEvent $ do 
      keyval <- eventKeyVal
      kn     <- eventKeyName 
      liftIO $ putStrLn kn
      m <- liftIO $ takeMVar mode
      case m of
        Command -> case kn of
                    "i" -> liftIO $ do putMVar mode Insert
                                       renderControl ui
                    _   -> liftIO $ putMVar mode m 
        Insert  -> case kn of
                    "Escape" -> liftIO $ do putMVar mode Command
                                            renderControl ui
                    _        -> do liftIO $ putMVar mode m
                                   stopEvent

    return ui

  mainLoop ui = mainGUI

  embedView view ui _ = do
    let tabHold = vimTabHolder ui
        meta    = TabMeta {} 
    
    embed view (embedHandle tabHold) (update ui meta) 
  
  replaceView view ui meta = do
    let tabHold  = vimTabHolder ui
        meta     = TabMeta {} 
        currentM = vimTabCurrent ui
    oldView <- tryTakeMVar currentM 
    maybe (return ()) destroy oldView 
    putMVar currentM view 
    embed view (embedHandle tabHold) (update ui meta)

  changedURI view ui meta = do
    uri <- getCurrentURI view 
    renderStatus ui uri 

  changedTitle view ui meta = return ()

  changedProgress percent ui meta = return ()

  changedStatus status ui meta = return () 


renderStatus :: VimUI -> URI -> IO () 
renderStatus ui uri =
 let status = vimStatus ui 
 in  do webViewLoadHtmlString status ("<body style=\"color:white;font-weight:bold;font-size:13px;background:black;margin:0;padding:0\">" ++ show uri ++ "</body>") "" 

renderControl :: VimUI -> IO ()
renderControl ui = do
  let status = vimControl ui
  m <- readMVar $ vimMode ui
  state <- case m of 
            Command -> return "" 
            Insert  -> return " -- INSERT -- "
  webViewLoadHtmlString status ("<body style=\"color:black;font-size:13px;background:white;margin:0;padding:0\">" ++ state ++ "</body>") "" 
 

embedHandle container widget = do
  mapM_ (containerRemove container) =<< containerGetChildren container
  containerAdd container widget 
  widgetShowAll container
