{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module LambdaCat.View.Web
    ( WebView
    , webView

    , module LambdaCat.View
    ) where

import LambdaCat.View
import LambdaCat.UI 
import LambdaCat.Supplier

import Data.Maybe
import Data.Typeable
import qualified Graphics.UI.Gtk.WebKit.WebView as WV
import Graphics.UI.Gtk.Abstract.Widget
-- import Graphics.UI.Gtk.WebKit.WebSettings
-- import Graphics.UI.Gtk.WebKit.WebFrame
-- import Graphics.UI.Gtk.WebKit.Download
import qualified Graphics.UI.Gtk.WebKit.NetworkRequest as NR
import qualified Graphics.UI.Gtk.WebKit.WebNavigationAction as NA
import Graphics.UI.Gtk hiding (populatePopup,widgetDestroy)
import Network.URI
-- import System.Directory
-- import System.FilePath
-- import System.Glib.GError

-- | Data type representing the view. Do not confuse this with WebKit's WebView.
newtype WebView = WebView { webViewWidget :: WV.WebView }
  deriving (Eq, Typeable)

webView :: View
webView = View (WebView undefined)

{-
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
    -- _ <- widget `on` pvigationPolicyDecisionRequestedrintRequested
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
-}

instance ViewClass WebView where
    new = do
        widget <- WV.webViewNew
        return WebView { webViewWidget = widget }

    embed wV@(WebView { webViewWidget = widget }) embedder callback = do
        -- Setup signal handling
        _ <- widget `on` WV.navigationPolicyDecisionRequested $ \ _wf nr na _wpd -> do            
          muri <- NR.networkRequestGetUri nr
          reason <- NA.webNavigationActionGetReason na
          case muri of 
            Just uri -> 
              case reason of 
                NA.WebNavigationReasonFormResubmitted -> return False -- this is not handled because of the form data
                NA.WebNavigationReasonLinkClicked -> do 
                  supplyForView callback replaceView (fromJust $ parseURI uri)
                  return True
                _ -> return False
            Nothing  -> return False

        _ <- widget `on` WV.newWindowPolicyDecisionRequested $ \ _wf nr _na _wpd -> do
          muri <- NR.networkRequestGetUri nr
          case muri of 
            Just uri -> supplyForView callback replaceView (fromJust $ parseURI uri)
            Nothing  -> return ()  
          return True

        _ <- widget `on` WV.titleChanged $ \ _wf _title -> callback (changedTitle $ View wV)
        _ <- widget `on` WV.loadFinished $ \ _wf -> callback (changedURI $ View wV)

        -- Embed widget 
        embedder $ castToWidget widget

    destroy WebView { webViewWidget = widget } = do 
        -- TODO: Unref WebKit's WebView.
        --WV.webViewLoadUri widget "about:blank"
        WV.webViewStopLoading widget
        widgetDestroy widget 

    load WebView { webViewWidget = widget } uri = do
        -- TODO: Write module for URI conversion
        WV.webViewLoadUri widget (uriToString id uri "")
        return True

    getCurrentURI WebView { webViewWidget = widget } = do
        mUriStr <- WV.webViewGetUri widget
        return $ fromMaybe nullURI $ do
            uriStr <- mUriStr
            parseURI uriStr

    getCurrentTitle WebView { webViewWidget = widget } = do
        mTitle <- WV.webViewGetTitle widget
        return $ fromMaybe "(Untitled)" mTitle
