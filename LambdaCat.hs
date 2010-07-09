module Main
    ( main )
where

import LambdaCat.UI.Glade
import qualified LambdaCat.UI as UI
import LambdaCat.Browser
import qualified LambdaCat.Page as Page 
import LambdaCat.Page.WebView
import LambdaCat.Page.Poppler
import Graphics.UI.Gtk.WebKit.WebView
import Network.URI

main :: IO ()
main = runGladeIO $ do
    ui <- UI.init :: GladeIO GladeUI 
    browser <- UI.newBrowser ui :: GladeIO GladeBrowser
    let pageList = [( (Page.new :: GladeIO WebView) >>= return .Page.Page,["http:","https:"])
                   ,( (Page.new :: GladeIO PopplerPage) >>= return .Page.Page,["file:"])]
    mpage <- Page.pageFromURI pageList (parseURI "file:///home/sargon/rapid_api.pdf")
    case mpage of
        (Just page) -> UI.embedPage ui browser page
        Nothing     -> return ()
    -- page <- Page.new :: GladeIO WebView
    UI.mainLoop ui
