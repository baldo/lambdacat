{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}

module LambdaCat.View.Web
    ( WebViewPage

    , webViewPage

    , newWrappablePage
    , newWithPage
    ) where

import Prelude hiding (log)

import LambdaCat.Class
import LambdaCat.Configure

import Paths_lambdacat

import Control.Monad
import Data.Typeable
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.WebSettings
import Graphics.UI.Gtk.WebKit.WebFrame
import Graphics.UI.Gtk.WebKit.Download
import Graphics.UI.Gtk.WebKit.NetworkRequest
import Graphics.UI.Gtk hiding (populatePopup)
import Network.URI
import System.Directory
import System.FilePath
import System.Glib.GError

newtype WebViewPage = WebViewPage { unWebViewPage :: WebView }
  deriving (Eq, Typeable)

instance HasWidget WebViewPage WebView where
    getWidget = unWebViewPage

-- TODO: nicer API please!
webViewPage :: View
webViewPage = View (undefined :: WebViewPage)

newWrappablePage :: IO WebViewPage
newWrappablePage = webViewNew >>= return . WebViewPage

newWithPage :: (UIClass ui, PageClass p, HasWidget p WebView) => p -> CallBack ui tabid -> IO ()
newWithPage page cb = do
    cb (\ ui tabid _ -> uriChanged ui tabid (View page))
    let widget = getWidget page
    _ <- widget `on` loadFinished  $ (\ _ -> cb (\ ui tabid _ -> uriChanged ui tabid (View page)))
    _ <- widget `on` loadFinished  $ (\ _ -> cb (\ ui tabid _ -> changedTitle ui tabid (View page)))
    -- _ <- widget `on` createWebView $ (\ _ -> createNew >>= return . unWebViewPage)

    _ <- widget `on` downloadRequested $ \ download -> do
        (Just uri) <- downloadGetUri download
        dest <- getDownloadDestinationURI uri
        $plog putStrLn $ "Download " ++ uri ++ " to " ++ dest
        downloadSetDestinationUri download dest
        return True

    _ <- widget `on` loadCommitted   $ \ _ -> return ()
    _ <- widget `on` progressChanged $ \ p -> cb (changedProgress p)
    _ <- widget `on` loadError $ \ _wf suri (GError dom code msg) -> do
                                    $plog putStrLn ("Error:  " ++ msg)
                                    $plog putStrLn ("Code:   " ++ show code)
                                    $plog putStrLn ("Domain: " ++ show dom)
                                    case (code, parseURIReference suri) of
                                        (101, Just uri) -> do -- Hopefully: URL cannot be shown
                                            mw <- pageFromProtocol cb
                                                                   (uriModifier lambdaCatConf)
                                                                   (pageList lambdaCatConf)
                                                                   (Just $ View page)
                                                                   (Just uri)
                                            case mw of
                                                Nothing -> return False
                                                Just (w, uri') -> do
                                                    cb (\ ui tabid bid -> replacePage ui bid tabid (View page) w)
                                                    _ <- load w uri'
                                                    return True
                                        _ -> return False
    _ <- widget `on` titleChanged $ \ _ newTitle -> $plog putStrLn newTitle
    _ <- widget `on` hoveringOverLink $ \ mtitle muri -> do
        $plog putStrLn $ (show mtitle) ++ " --> " ++ (show muri)
        case muri of
            Just uri -> cb (statusChanged uri)
            Nothing  -> cb (statusChanged "")
        -- segfaults included - still?
    _ <- widget `on` webViewReady $ $plog putStrLn "Yay, I am ready" >> return True
    _ <- widget `on` closeWebView $ $plog putStrLn "CloseMe" >> return True
    -- _ <- widget `on` consoleMessage ...
    -- _ <- widget `on` copyClipboard
    -- _ <- widget `on` cutClipboard
    -- _ <- widget `on` pasteClipboard
    _ <- widget `on` populatePopup $ \ menu -> do
        $plog putStrLn "populatePopup"
        item <- menuItemNewWithLabel "Toggle source view"
        sep  <- separatorMenuItemNew

        _ <- onActivateLeaf item $ do
            let wv = getWidget page
            m <- webViewGetViewSourceMode wv
            webViewSetViewSourceMode wv $ not m
            reload page

        menuAttach menu item 0 1 0 1
        menuAttach menu sep 0 1 1 2

        widgetShow item
        widgetShow sep
    -- _ <- widget `on` printRequested
    -- _ <- widget `on` scriptAlert
    -- _ <- widget `on` scriptConfirm
    -- _ <- widget `on` scriptPrompt
    _ <- widget `on` statusBarTextChanged $ \ stat -> cb (statusChanged stat)
    -- _ <- widget `on` selectAll
    -- _ <- widget `on` selectionChanged
    -- _ <- widget `on` setScrollAdjustments
    -- _ <- widget `on` databaseQuotaExceeded
    _ <- widget `on` documentLoadFinished $ \ wf -> do
        muri <- webFrameGetUri wf
        case muri of
            Just uri ->
                $plog putStrLn $ "documentLoadFinished: "
                              ++ show uri
                              ++ " => "
                              ++ dshow (parseURIReference uri)
            Nothing ->
                $plog putStrLn $ "documentLoadFinished, but not successfull."

    _ <- widget `on` iconLoaded $ \ uri -> do
        $plog putStrLn $ "Icon:" ++ (show uri)
{- Needs patch in webkit
        let wv = getWidget page
        frame <- webViewGetMainFrame wv
        dsrc <- webFrameGetDataSource frame

        rs <- webDataSourceGetSubresources dsrc
        ricos <- filterM (fmap (== uri) . webResourceGetUri) rs

        case ricos of
            (rico : _) -> do
                $plog putStrLn "here webResourceGetData should be used..."
            [] -> $pinfo putStrLn "Icon: Strange, no icon resource found..."
-}
    -- _ <- widget `on` redo -- binding didn't match webkitgtk signal
    -- _ <- widget `on` undo -- binding didn't match webkitgtk signal
    _ <- widget `on` mimeTypePolicyDecisionRequested $ \ _wf nr mime _wp -> do
        when (mime == "application/rss+xml" || mime == "application/atom+xml") $ do
            let wv = getWidget page
            let fname = takeWhile (/= '+') $ tail $ dropWhile (/= '/') mime
            css <- getDataFileName $ "styles/" ++ fname ++ ".css"
            ws <- webViewGetWebSettings wv
            set ws [ webSettingsUserStylesheetUri := Just $ "file://" ++ css ]
            webViewSetWebSettings wv ws
        $plog putStrLn $ "Mime: " ++ mime
        msuri <- networkRequestGetUri nr
        case msuri of
            Just suri -> do
                let Just uri = parseURI suri -- shouldn't fail
                maybePage <- pageFromMimeType cb mime (mimeList lambdaCatConf)
                case maybePage of
                    Just newPage -> do
                        $plog putStr ("Mime: uri = " ++ show uri ++ " => " ++ dshow uri)
                        cb (\ ui tid bid -> replacePage ui bid tid (View page) newPage)
                        _ <- load newPage uri
                        return True
                    Nothing -> return False
            Nothing -> return False
    -- _ <- widget `on` moveCursor
    -- _ <- widget `on` navigationPolicyDecisionRequested
    -- _ <- widget `on` newWindowPolicyDecisionRequested
    _ <- widget `on` resourceRequestStarting $ \ wf wr mnrequ nresp -> do
        case mnrequ of
            Just nrequ -> do
                requestUri <- networkRequestGetUri nrequ
                $plog putStrLn $ "ResourceRequest: " ++ (show requestUri) 
            Nothing -> return ()
    -- _ <- widget `on` geolocationPolicyDecisionCancelled
    -- _ <- widget `on` geolocationPolicyDecisionRequested
    return ()
{-
  where createNew :: IO WebViewPage
        createNew = do
            page <- new cb
            cb (\ ui bid -> embedPage ui bid (View page))
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

    search page text = do
        let wv = getWidget page
        webViewUnMarkTextMatches wv
        _ <- webViewMarkTextMatches wv text False 0
        webViewSetHighlightTextMatches wv True
        _ <- webViewSearchText wv text False True True
        return ()

getDownloadDestinationURI :: String -> IO String
getDownloadDestinationURI uri = do
    appDir <- getAppUserDataDirectory "lambdacat"
    let webCache =  appDir </>  "webView"
    createDirectoryIfMissing True webCache
    return $ "file://" ++ webCache </> (makeValid . filter isUnreserved) (escapeURIString isUnreserved uri)
