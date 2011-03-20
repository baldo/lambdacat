{-# LANGUAGE FlexibleInstances
           , MultiParamTypeClasses
           , RankNTypes
           , TypeFamilies
           , TypeSynonymInstances
  #-}

{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

-- |
-- Module      : LambdaCat.View.Vim
-- Copyright   : Andreas Baldeau, Daniel Ehlers
-- License     : BSD3
-- Maintainer  : Andreas Baldeau <andreas@baldeau.net>,
--               Daniel Ehlers <danielehlers@mindeye.net>
-- Stability   : Alpha
--
-- This module is a first proof of concept for an Vim like UI.

module LambdaCat.UI.Vim
    (
      -- * Datatype
      VimUI
    , vimUIConf

      -- * Module exports
    , module LambdaCat.UI
    )
where

import Control.Concurrent.MVar
import Control.Monad.Reader
import Network.URI

import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.WebView as WebView

import LambdaCat.Session
import LambdaCat.Supplier
import LambdaCat.UI
import LambdaCat.UI.Glade.PersistentTabId
import LambdaCat.Utils.InputBuffer

-- | The VimUI datatype.
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

-- | Metadata to store with tab.
data TabMeta = TabMeta
    {- tabMetaIdent     :: TabId
    , tabMetaLabel     :: Label
    , tabMetaImage     :: Image
    , tabMetaContainer :: Container
    -}

-- | Default 'VimUI' configuration.
vimUIConf :: UIConf VimUI TabMeta
vimUIConf = VimConf

-- | Modes of operation.
data Mode = Command InputBuffer
          | Insert
          | Normal
  deriving Show

instance UIClass VimUI TabMeta where

    data UIConf VimUI TabMeta = VimConf

    init _uiConf = do
        _ <- initGUI

        window <- windowNew
        vbox   <- vBoxNew False 0
        tabHolder <- hBoxNew False 0

        containerAdd window vbox

        mode <- newMVar Normal

        -- window settings
        _ <- onDestroy window mainQuit


        -- status bar
        status <- webViewNew
        widgetSetSizeRequest status (-1) 15

        -- control / input area
        control <- webViewNew
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

        -- initial rendering
        renderStatus ui nullURI
        renderControl ui

        -- simple keyboard handling
        _ <- window `on` keyPressEvent $ tryEvent $ do
            kv <- eventKeyVal
            let kn  = keyName kv
            let mkc = keyToChar kv
            liftIO $ do
                putStrLn $ kn ++ " -> " ++ show mkc
                m <- takeMVar mode
                m' <- handleKeyPress m $ maybe kn (:[]) mkc
                putMVar mode m'
                renderControl ui

        return ui

    mainLoop _ui = mainGUI

    embedView view ui _ = do
        let tabHold = vimTabHolder ui
            meta    = TabMeta {}

        embed view (embedHandle tabHold) (update ui meta)

    replaceView view ui _meta = do
        let tabHold  = vimTabHolder ui
            meta     = TabMeta {}
            currentM = vimTabCurrent ui
        oldView <- tryTakeMVar currentM
        maybe (return ()) destroy oldView
        putMVar currentM view
        embed view (embedHandle tabHold) (update ui meta)

    updateView view URIChanged ui _meta = do
        uri <- getCurrentURI view
        renderStatus ui uri
    updateView _view TitleChanged _ui _meta =
        return ()
    updateView _view (ProgressChanged _percent) _ui _meta =
        return ()
    updateView _view (StatusChanged _status) _ui _meta =
        return ()

handleKeyPress :: Mode -> String -> IO Mode
handleKeyPress (Command _buffer) "Escape" =
    return Normal

handleKeyPress (Command buffer) "Return" = do
    putStrLn $ toString buffer
    return Normal

handleKeyPress (Command buffer)  "Left" =
    return $ Command $ left buffer
handleKeyPress (Command buffer) "Right" =
    return $ Command $ right buffer
handleKeyPress (Command buffer) "Home" =
    return $ Command $ home buffer
handleKeyPress (Command buffer) "End" =
    return $ Command $ end buffer

handleKeyPress (Command buffer) "Delete" =
    return $ Command $ delete buffer
handleKeyPress (Command buffer) "BackSpace" =
    return $ Command $ backSpace buffer

handleKeyPress (Command buffer) [c] =
    return $ Command $ insert c buffer

handleKeyPress Insert "Escape" =
    return Normal

handleKeyPress Normal ":" =
    return $ Command empty
handleKeyPress Normal "i" =
    return Insert

handleKeyPress m _kn =
    return m

renderStatus :: VimUI -> URI -> IO ()
renderStatus ui uri = do
    let status = vimStatus ui
    webViewLoadHtmlString status
        ( "<body style=\"color:white;font-weight:bold;font-size:13px;"
       ++ "background:black;margin:0;padding:0\">" ++ show uri ++ "</body>"
        ) ""

renderControl :: VimUI -> IO ()
renderControl ui = do
    let status = vimControl ui

    m <- readMVar $ vimMode ui

    state <- case m of
                 Normal ->
                     return ""

                 Command buffer -> do
                     let (onCrsr, aftrCrsr) =
                             case afterCursor buffer of
                                 "" ->
                                     ("&nbsp;", "")
                                 c : cs ->
                                     (htmlEncode c, cs)
                     return
                         ( ':' : htmlEncodeString (beforeCursor buffer)
                        ++ renderCursor onCrsr
                        ++ htmlEncodeString aftrCrsr
                         )

                 Insert ->
                     return " -- INSERT -- "

    webViewLoadHtmlString status
        ( "<body style=\"color:black;font-size:13px;background:white;"
       ++ "font-family:monospace;margin:0;padding:0\">"
       ++ state
       ++ "</body>"
        ) ""

htmlEncodeString :: String -> String
htmlEncodeString = concatMap htmlEncode

htmlEncode :: Char -> String
htmlEncode ' ' = "&nbsp;"
htmlEncode '&' = "&amp;"
htmlEncode '<' = "&lt;"
htmlEncode c   = [c]

renderCursor :: String -> String
renderCursor str =
    "<span style=\"color:white;background-color:black\">" ++ str ++ "</span>"

embedHandle :: (ContainerClass container, WidgetClass widget)
            => container -> widget -> IO ()
embedHandle container widget = do
    cs <- containerGetChildren container
    mapM_ (containerRemove container) cs
    containerAdd container widget
    widgetShowAll container

