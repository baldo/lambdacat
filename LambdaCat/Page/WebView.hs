{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

module LambdaCat.Page.WebView
    ( WebViewPage

    , webViewPage
    ) where

import Prelude hiding (log)

import LambdaCat.Page
import LambdaCat.Configure
import LambdaCat.Utils

import Data.Typeable
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.WebFrame
import Graphics.UI.Gtk.WebKit.Download
-- import Graphics.UI.Gtk.WebKit.NetworkRequest
import Graphics.UI.Gtk
import Network.URI
import System.Directory
import System.FilePath

newtype WebViewPage = WebViewPage { unWebViewPage :: WebView }
  deriving (Eq, Typeable)

instance HasWidget WebViewPage WebView where
    getWidget = unWebViewPage

webViewPage :: Page
webViewPage = Page (undefined :: WebViewPage)

instance PageClass WebViewPage where
    new cb = do
        page <- webViewNew >>= return . WebViewPage
        cb (\ui _ -> uriChanged ui (Page page))
        let widget = getWidget page
        _ <- widget `on` loadFinished  $ (\ _ -> cb (\ui _ -> uriChanged ui (Page page)))
        _ <- widget `on` loadFinished  $ (\ _ -> cb (\ui _ -> changedTitle ui (Page page)))
        _ <- widget `on` createWebView $ (\ _ -> createNew >>= return . unWebViewPage)

        _ <- widget `on` downloadRequested $ \ download -> do
            (Just uri) <- downloadGetUri download
            dest <- getDownloadDestinationURI uri
            log putStrLn $ "Download " ++ uri ++ " to " ++ dest
            downloadSetDestinationUri download dest
            return True

        _ <- widget `on` loadCommitted   $ \ _ -> return ()
        _ <- widget `on` progressChanged $ \ p -> log putStrLn $ "Progress:" ++ (show p)
        _ <- widget `on` loadError $ \ _ err _ -> log putStrLn err >> return False
        _ <- widget `on` titleChanged $ \ _ newTitle -> log putStrLn newTitle
        --  _ <- widget `on` hoveringOverLink $ \ a b -> log putStrLn $ a ++ " --> " ++ b -- segfaults included
        _ <- widget `on` webViewReady $ log putStrLn "Yay, I am ready" >> return True
        _ <- widget `on` closeWebView $ log putStrLn "CloseMe" >> return True
        --  _ <- widget `on` consoleMessage ...
        --  _ <- widget `on` copyClipboard
        --  _ <- widget `on` cutClipboard
        --  _ <- widget `on` pasteClipboard
        --  _ <- widget `on` populatePopup
        --  _ <- widget `on` printRequested
        --  _ <- widget `on` scriptAlert
        --  _ <- widget `on` scriptConfirm
        --  _ <- widget `on` scriptPrompt
        _ <- widget `on` statusBarTextChanged $ \ str -> log putStrLn $ "Status:" ++ str
        --  _ <- widget `on` selectAll
        --  _ <- widget `on` selectionChanged
        --  _ <- widget `on` setScrollAdjustments
        --  _ <- widget `on` databaseQuotaExceeded
        _ <- widget `on` documentLoadFinished $ \ wf -> do 
            uri <- webFrameGetUri wf
            log putStrLn $ "documentLoadFinished: " ++ (show uri)
        -- _ <- widget `on` iconLoaded $ log putStrLn $ "Icon loaded" -- segfaults included 
        -- _ <- widget `on` redo -- binding didn't match webkitgtk signal
        -- _ <- widget `on` undo -- binding didn't match webkitgtk signal
        _ <- widget `on` mimeTypePolicyDecisionRequested $ \ _wf _nr mime _wp -> do
            log putStrLn $ "Mime: " ++ mime
            maybePage <- pageFromMimeType cb mime (mimeList lambdaCatConf)
            case maybePage of
                Just newPage -> do
                    cb (\ ui bid -> replacePage ui bid (Page page) newPage)
                    return True
                Nothing -> return False
        --  _ <- widget `on` moveCursor
        --  _ <- widget `on` navigationPolicyDecisionRequested
        --  _ <- widget `on` newWindowPolicyDecisionRequested
        {- _ <- widget `on` resourceRequestStarting $ \ wf wr nrequ nresp -> do
            requestUri <- networkRequestGetUri nrequ 
            putStrLn $ "ResourceRequest: " ++ (show requestUri) -} -- makeNewGObjectError / NullPointerProblem
        --  _ <- widget `on` geolocationPolicyDecisionCancelled
        --  _ <- widget `on` geolocationPolicyDecisionRequested

        return page
      where createNew :: IO WebViewPage
            createNew = do
                page <- new cb
                cb (\ ui bid -> embedPage ui bid (Page page))
                return page

    destroy page = do
        webViewLoadHtmlString (getWidget page) "text/html" ""
        

    load page uri =  webViewLoadUri (unWebViewPage page) uriString >> return True
        where uriString = uriToString id uri ""

    getCurrentURI page = do
        muri <- webViewGetUri.unWebViewPage $ page
        case muri of
            Just uri -> case parseURI uri of
                Just x -> return x
                _ -> return nullURI
            _ -> return nullURI

    getCurrentTitle page = do
        mTitle <- webViewGetTitle.unWebViewPage $ page
        case mTitle of
            Just title -> return title
            _ -> return ""

    back    =  webViewGoBack . unWebViewPage
    forward =  webViewGoForward . unWebViewPage
    stop    =  webViewStopLoading . unWebViewPage
    reload  =  webViewReload . unWebViewPage


getDownloadDestinationURI :: String -> IO String
getDownloadDestinationURI uri = do
    appDir <- getAppUserDataDirectory "lambdacat"
    let webCache =  appDir </>  "webView"
    createDirectoryIfMissing True webCache
    return $ "file://" ++ webCache </> (makeValid.filter isUnreserved) (escapeURIString isUnreserved uri)
