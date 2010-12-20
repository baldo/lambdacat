{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module LambdaCat.View.About
    ( AboutPage

    , aboutPage
    ) where

import LambdaCat.Class
import LambdaCat.View.WebView ( WebViewPage, newWrappablePage, newWithPage )

import Paths_lambdacat

import Control.Concurrent.MVar
import Data.Typeable
import Graphics.UI.Gtk.WebKit.WebView
import Network.URI

data AboutPage = AboutPage
    { webViewPage :: WebViewPage
    , aboutUri :: MVar URI
    }
  deriving (Eq, Typeable)

aboutPage :: View
aboutPage = View (undefined :: AboutPage)

instance HasWidget AboutPage WebView where
    getWidget = getWidget . webViewPage

instance PageClass AboutPage where
-- TODO: nicer API please!
    new cb = do
        uri <- newMVar nullURI
        wvpage <- newWrappablePage
        let page = AboutPage { webViewPage = wvpage, aboutUri = uri }
        newWithPage page cb
        return page

    destroy cb = destroy (webViewPage cb)

    load page uri = do
        _ <- swapMVar (aboutUri page) uri
        curi <- aboutToFileURI uri
        load (webViewPage page) curi

    getCurrentURI AboutPage { aboutUri = curi } = readMVar curi

    getCurrentTitle = getCurrentTitle . webViewPage

aboutToFileURI :: URI -> IO URI
aboutToFileURI URI { uriScheme = "about:", uriPath = name } = do
    let safeName = escapeURIString (flip elem $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']) name
    fpath <- getDataFileName $ "about/" ++ safeName ++ ".html"
    return $ filePathToURI fpath
aboutToFileURI _ = error "Invalid about-URI."

filePathToURI :: FilePath -> URI
filePathToURI fp = URI
    { uriScheme    = "file:"
    , uriAuthority = Nothing
    , uriPath      = fp
    , uriQuery     = ""
    , uriFragment  = ""
    }

