{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

module LambdaCat.Page.Cat
    ( CatPage

    , catPage
    )
where

import LambdaCat.Page
import LambdaCat.Page.WebView ( WebViewPage )

import Paths_lambdacat

import Control.Concurrent
import Data.Typeable
import Graphics.UI.Gtk.WebKit.WebView
import Network.URI

data CatPage = CatPage
    { webViewPage :: WebViewPage
    , catUri :: MVar URI
    }
  deriving (Eq, Typeable)

catPage :: Page
catPage = Page (undefined :: CatPage)

instance HasWidget CatPage WebView where
    getWidget = getWidget . webViewPage

instance PageClass CatPage where
    new cb = do
        wvpage <- new cb
        uri <- newMVar nullURI
        return $ CatPage { webViewPage = wvpage, catUri = uri }

    destroy cb = destroy (webViewPage cb) 

    load page uri = do
        _ <- swapMVar (catUri page) uri
        curi <- catToFileURI uri
        load (webViewPage page) curi

    getCurrentURI CatPage { catUri = curi } = readMVar curi

    getCurrentTitle = getCurrentTitle . webViewPage

catToFileURI :: URI -> IO URI
catToFileURI URI { uriScheme = "cat:", uriPath = name } = do
    let safeName = escapeURIString (flip elem $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']) name
    fpath <- getDataFileName $ "cats/" ++ safeName ++ ".jpg"
    return $ filePathToURI fpath
catToFileURI _ = error "Invalid cat-URI."

filePathToURI :: FilePath -> URI
filePathToURI fp = URI
    { uriScheme    = "file:"
    , uriAuthority = Nothing
    , uriPath      = fp
    , uriQuery     = ""
    , uriFragment  = ""
    }

