{-# LANGUAGE FlexibleContexts #-}
module Main
    ( main )
where

import LambdaCat.UI.Glade
import qualified LambdaCat.UI as UI
import LambdaCat.Browser
import LambdaCat.Page
import Graphics.UI.Gtk.WebKit.WebView

main :: IO ()
main = do
    ui <- UI.init :: IO GladeUI 
    browser <- UI.newBrowser ui :: IO GladeBrowser
    UI.mainLoop ui
