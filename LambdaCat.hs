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
main = runGladeIO $ do
    ui <- UI.init :: GladeIO GladeUI 
    browser <- UI.newBrowser ui :: GladeIO GladeBrowser
    page <- Page.new :: GladeIO WebView
    UI.embedPage ui browser page 
    UI.mainLoop ui
