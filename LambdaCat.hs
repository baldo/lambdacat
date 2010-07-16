module Main
    ( main )
where

import LambdaCat.UI.Glade
--import qualified LambdaCat.UI as UI
import LambdaCat.Browser
import qualified LambdaCat.Page as Page
import qualified LambdaCat.Page as UI
import LambdaCat.Page.WebView
import LambdaCat.Page.Poppler
import Graphics.UI.Gtk.WebKit.WebView
import Network.URI
import System
import Data.Maybe

main :: IO ()
main = do
    args <- getArgs
    let uri = if null args
              then "http://www.haskell.org"
              else  head args
    runGladeIO $ do
        ui <- UI.init :: GladeIO GladeUI
        browser <- UI.newBrowser ui :: GladeIO BrowserID
        let pageList = [ (Page.Page (undefined :: WebViewPage), ["http:","https:"])
                       , (Page.Page (undefined :: PopplerPage), ["file:"])
                       ]
        mpage <- Page.pageFromProtocol (UI.update ui) pageList Nothing (parseURI uri)
        case mpage of
            (Just page) -> do
                Page.load page (fromJust $ parseURI uri)
                UI.embedPage ui browser page
            Nothing     -> return ()
        -- page <- Page.new :: GladeIO WebView
        UI.mainLoop ui
