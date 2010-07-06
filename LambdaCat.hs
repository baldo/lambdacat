{-# LANGUAGE FlexibleContexts #-}
module Main
    ( main )
where

import LambdaCat.UI.Glade
import qualified LambdaCat.UI as UI
import LambdaCat.Browser
import qualified LambdaCat.Page as Page 
import Graphics.UI.Gtk.WebKit.WebView
import Network.URI

main :: IO ()
main = runGladeIO $ do
    ui <- UI.init :: GladeIO GladeUI 
    browser <- UI.newBrowser ui :: GladeIO GladeBrowser
    let pageList = [(Page.new :: GladeIO WebView,["http:","https:"])]
    mpage <- Page.pageFromURI pageList (parseURI "http://www.plagis.de")
    case mpage of
        (Just page) -> UI.embedPage ui browser page 
        Nothing     -> return ()
    -- page <- Page.new :: GladeIO WebView
    UI.mainLoop ui
