{-# LANGUAGE FlexibleContexts #-}
module Main
    ( main )
where

import LambdaCat.UI.Glade
import qualified LambdaCat.UI as UI
import LambdaCat.Browser
import qualified LambdaCat.Page as Page 
import Graphics.UI.Gtk.WebKit.WebView

main :: IO ()
main = do
    ui <- UI.init :: IO GladeUI 
    browser <- UI.newBrowser ui :: IO GladeBrowser
    page <- Page.new :: IO WebView
    UI.embedPage ui browser page 
    UI.mainLoop ui
