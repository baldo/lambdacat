{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}

module LambdaCat.Page.WebView
    ( WebViewPage

    , webViewPage

    , newWrappablePage
    , newWithPage
    ) where

import Prelude hiding (log)

import LambdaCat.Page
import LambdaCat.Configure
import LambdaCat.Utils

import Data.Typeable
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.WebFrame
import Graphics.UI.Gtk.WebKit.Download
import Graphics.UI.Gtk.WebKit.NetworkRequest
import Graphics.UI.Gtk
import Network.URI
import System.Directory
import System.FilePath
import System.Glib.GError

newtype WebViewPage = WebViewPage { unWebViewPage :: WebView }
  deriving (Eq, Typeable)

instance HasWidget WebViewPage WebView where
    getWidget = unWebViewPage

-- TODO: nicer API please!
webViewPage :: Page
webViewPage = Page (undefined :: WebViewPage)

newWrappablePage :: IO WebViewPage
newWrappablePage = webViewNew >>= return . WebViewPage

newWithPage :: (UIClass ui, PageClass p, HasWidget p WebView) => p -> CallBack ui -> IO ()
newWithPage page cb = do
    cb (\ ui _ -> uriChanged ui (Page page))
    let widget = getWidget page
    _ <- widget `on` loadFinished  $ (\ _ -> cb (\ ui _ -> uriChanged ui (Page page)))
    _ <- widget `on` loadFinished  $ (\ _ -> cb (\ ui _ -> changedTitle ui (Page page)))
    -- _ <- widget `on` createWebView $ (\ _ -> createNew >>= return . unWebViewPage)

    _ <- widget `on` downloadRequested $ \ download -> do
        (Just uri) <- downloadGetUri download
        dest <- getDownloadDestinationURI uri
        $plog putStrLn $ "Download " ++ uri ++ " to " ++ dest
        downloadSetDestinationUri download dest
        return True

    _ <- widget `on` loadCommitted   $ \ _ -> return ()
    _ <- widget `on` progressChanged $ \ p -> $plog putStrLn $ "Progress:" ++ show p
    _ <- widget `on` loadError $ \ _wf suri (GError dom code msg) -> do
                                    $plog putStrLn ("Error:  " ++ msg)
                                    $plog putStrLn ("Code:   " ++ show code)
                                    $plog putStrLn ("Domain: " ++ show dom)
                                    case (code, parseURIReference suri) of
                                        (101, Just uri) -> do -- Hopefully: URL cannot be shown
                                            mw <- pageFromProtocol cb
                                                                   (uriModifier lambdaCatConf)
                                                                   (pageList lambdaCatConf)
                                                                   (Just $ Page page)
                                                                   (Just uri)
                                            case mw of
                                                Nothing -> return False
                                                Just (w, uri') -> do
                                                    cb (\ ui bid -> replacePage ui bid (Page page) w)
                                                    _ <- load w uri'
                                                    return True
                                        _ -> return False
    _ <- widget `on` titleChanged $ \ _ newTitle -> $plog putStrLn newTitle
    -- _ <- widget `on` hoveringOverLink $ \ a b -> $plog putStrLn $ a ++ " --> " ++ b -- segfaults included
    _ <- widget `on` webViewReady $ $plog putStrLn "Yay, I am ready" >> return True
    _ <- widget `on` closeWebView $ $plog putStrLn "CloseMe" >> return True
    -- _ <- widget `on` consoleMessage ...
    -- _ <- widget `on` copyClipboard
    -- _ <- widget `on` cutClipboard
    -- _ <- widget `on` pasteClipboard
    -- _ <- widget `on` populatePopup
    -- _ <- widget `on` printRequested
    -- _ <- widget `on` scriptAlert
    -- _ <- widget `on` scriptConfirm
    -- _ <- widget `on` scriptPrompt
    _ <- widget `on` statusBarTextChanged $ \ str -> $plog putStrLn $ "Status:" ++ str
    -- _ <- widget `on` selectAll
    -- _ <- widget `on` selectionChanged
    -- _ <- widget `on` setScrollAdjustments
    -- _ <- widget `on` databaseQuotaExceeded
    _ <- widget `on` documentLoadFinished $ \ wf -> do
        uri <- webFrameGetUri wf
        $plog putStrLn $ "documentLoadFinished: " ++ show uri
    -- _ <- widget `on` iconLoaded $ $plog putStrLn $ "Icon loaded" -- segfaults included
    -- _ <- widget `on` redo -- binding didn't match webkitgtk signal
    -- _ <- widget `on` undo -- binding didn't match webkitgtk signal
    _ <- widget `on` mimeTypePolicyDecisionRequested $ \ _wf nr mime _wp -> do
        $plog putStrLn $ "Mime: " ++ mime
        msuri <- networkRequestGetUri nr
        case msuri of
            Just suri -> do
                let Just uri = parseURI suri -- shouldn't fail
                maybePage <- pageFromMimeType cb mime (mimeList lambdaCatConf)
                case maybePage of
                    Just newPage -> do
                        $plog putStr ("Mime: uri = " ++ show uri)
                        cb (\ ui bid -> replacePage ui bid (Page page) newPage)
                        _ <- load newPage uri
                        return True
                    Nothing -> return False
            Nothing -> return False
    -- _ <- widget `on` moveCursor
    -- _ <- widget `on` navigationPolicyDecisionRequested
    -- _ <- widget `on` newWindowPolicyDecisionRequested
    {- _ <- widget `on` resourceRequestStarting $ \ wf wr nrequ nresp -> do
        requestUri <- networkRequestGetUri nrequ
        putStrLn $ "ResourceRequest: " ++ (show requestUri) -} -- makeNewGObjectError / NullPointerProblem
    -- _ <- widget `on` geolocationPolicyDecisionCancelled
    -- _ <- widget `on` geolocationPolicyDecisionRequested

    return ()
{-
  where createNew :: IO WebViewPage
        createNew = do
            page <- new cb
            cb (\ ui bid -> embedPage ui bid (Page page))
            return page
-}

instance PageClass WebViewPage where
    new cb = do
        page <- newWrappablePage
        newWithPage page cb
        return page

    destroy page =
        webViewLoadHtmlString (getWidget page) "text/html" ""

    load page uri =  webViewLoadUri (unWebViewPage page) uriString >> return True
        where uriString = uriToString id uri ""

    getCurrentURI page = do
        muri <- webViewGetUri . unWebViewPage $ page
        case muri of
            Just uri -> case parseURI uri of
                Just x -> return x
                _ -> return nullURI
            _ -> return nullURI

    getCurrentTitle page = do
        mTitle <- webViewGetTitle . unWebViewPage $ page
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
    return $ "file://" ++ webCache </> (makeValid . filter isUnreserved) (escapeURIString isUnreserved uri)
