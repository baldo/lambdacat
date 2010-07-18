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
import LambdaCat.Page.MPlayer
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
                       , (Page.Page (undefined :: MPlayerPage), ["mms:"])
                       ]
        mpage <- Page.pageFromProtocol (UI.update ui browser) pageList Nothing (parseURI uri)
        case mpage of
            (Just page) -> do
                UI.embedPage ui browser page
                Page.load page (fromJust $ parseURI uri)
                return ()
            Nothing     -> return ()
        mpage2 <- Page.pageFromProtocol (UI.update ui browser ) pageList Nothing (parseURI uri)
        case mpage2 of
            (Just page) -> do
                UI.embedPage ui browser page
                Page.load page (fromJust $ parseURI uri)
                return ()
            Nothing     -> return ()

        -- page <- Page.new :: GladeIO WebView
        UI.mainLoop ui
