{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

module LambdaCat.Page.Cat
    ( CatPage
    )
where

import LambdaCat.Page
import LambdaCat.Page.WebView

import Paths_lambdacat

import Control.Concurrent
import Control.Monad.Trans
import Data.Typeable
import Graphics.UI.Gtk.WebKit.WebView
import Network.URI

data CatPage = CatPage
    { webViewPage :: WebViewPage
    , catUri :: MVar URI
    }
  deriving (Eq, Typeable)

instance HasWidget CatPage WebView where
    getWidget = getWidget . webViewPage

instance SinkMonad m => PageClass CatPage m where 
    new cb = do
        wvpage <- new cb
        uri <- liftIO $ newMVar nullURI
        return $ CatPage { webViewPage = wvpage, catUri = uri }

    load page uri = do
        _ <- liftIO $ swapMVar (catUri page) uri
        curi <- liftIO $ catToFileURI uri
        load (webViewPage page) curi

    getCurrentURI CatPage { catUri = curi } = liftIO $
        readMVar curi

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

