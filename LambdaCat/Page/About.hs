{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module LambdaCat.Page.About
    ( AboutPage

    , aboutPage
    ) where

import LambdaCat.Page
import LambdaCat.Page.WebView ( WebViewPage )

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

aboutPage :: Page
aboutPage = Page (undefined :: AboutPage)

instance HasWidget AboutPage WebView where
    getWidget = getWidget . webViewPage

instance PageClass AboutPage where
    new cb = do
        wvpage <- new cb
        uri <- newMVar nullURI
        return $ AboutPage { webViewPage = wvpage, aboutUri = uri }
    
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

