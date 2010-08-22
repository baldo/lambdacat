{-# LANGUAGE CPP #-}

module LambdaCat
    ( lambdacat
    , defaultConfig
    , LambdaCatConf (..)
    )
where

import LambdaCat.Browser
import LambdaCat.Configure
import LambdaCat.CmdArgs
import LambdaCat.Page.Cat
import LambdaCat.Page.About
import LambdaCat.Page.MPlayer
import LambdaCat.Page.Poppler
import LambdaCat.Page.WebView
import LambdaCat.UI.Glade
import qualified LambdaCat.Page as Page
import qualified LambdaCat.Page as UI

import Config.Dyre
import Config.Dyre.Compile
import Data.Maybe
import Network.URI
import System
import System.IO

defaultURIModifier :: URI -> URI
defaultURIModifier uri
    | not $ null $ uriScheme uri = uri
    | otherwise                  =
        let p = case show uri of
                    ('/' : _) -> "file://"
                    _         -> "http://"
            Just uri' = parseURI $ p ++ show uri
        in  uri'

defaultConfig :: LambdaCatConf
defaultConfig = LambdaCatConf
    { uriModifier = defaultURIModifier
    , pageList    = [ (webViewPage, ["http:", "https:", "file:"])
                    , (popplerPage, ["file:"])
                    , (mplayerPage, ["mms:", "http:", "file:"])
                    , (catPage    , ["cat:"])
                    , (aboutPage  , ["about:"])
                    ]
    , mimeList    = [ (popplerPage, ["application/pdf"])
                    , (mplayerPage, [ "application/asx"
                                    , "application/ogg"
                                    , "application/x-flac"
                                    , "application/x-mplayer2"
                                    , "application/x-ogg"
                                    , "application/x-quicktimeplayer"
                                    , "audio/mp3"
                                    , "audio/mpeg"
                                    , "audio/mpeg-url"
                                    , "audio/mpeg2"
                                    , "audio/mpeg3"
                                    , "audio/mpegurl"
                                    , "audio/x-flac"
                                    , "audio/x-mp3"
                                    , "audio/x-mpeg"
                                    , "audio/x-mpeg-url"
                                    , "audio/x-mpeg2"
                                    , "audio/x-mpeg3"
                                    , "audio/x-mpegurl"
                                    , "audio/x-ms-wax"
                                    , "audio/x-ms-wma"
                                    , "audio/x-ogg"
                                    , "audio/x-scpls"
                                    , "image/x-macpaint"
                                    , "video/fli"
                                    , "video/mp4"
                                    , "video/msvideo"
                                    , "video/ogg"
                                    , "video/quicktime"
                                    , "video/theora"
                                    , "video/x-fli"
                                    , "video/x-ms-asf"
                                    , "video/x-ms-asf-plugin"
                                    , "video/x-ms-wm"
                                    , "video/x-ms-wmv"
                                    , "video/x-ms-wvx"
                                    , "video/x-msvideo"
                                    , "video/x-ogg"
                                    , "video/x-quicktime"
                                    , "video/x-theora"
                                    ])
                    ]
    , homeURI     = fromJust $ parseURI "http://www.haskell.org"
    }

mainCat :: (Maybe String, LambdaCatConf) -> IO ()
mainCat (e, cfg) = do
    maybe (return ()) error e

    setLCC cfg
    args <- getCmdArgs

    let us = if null $ uris args
             then [show $ homeURI cfg]
             else uris args
    ui <- UI.init :: IO GladeUI
    browser <- UI.newBrowser ui :: IO BrowserId
    mapM_ (\ uri -> do
        mpage <- Page.pageFromProtocol (UI.update ui browser) (uriModifier lambdaCatConf) (pageList lambdaCatConf) Nothing (parseURIReference uri)
        case mpage of
            (Just (page, uri')) -> do
                UI.embedPage ui browser page
                _ <- Page.load page uri'
                return ()
            Nothing     -> return ()
        ) us
    UI.mainLoop ui

lambdacat :: LambdaCatConf -> IO ()
lambdacat cfg = do
    args <- getCmdArgs

    if recompile args
        then do
            customCompile dparams
            me <- getErrorString dparams
            case me of
                Just e  -> do
                    hPutStrLn stderr e
                    exitFailure
                Nothing -> return ()
        else wrapMain dparams (Nothing, cfg)

dparams :: Params (Maybe String, LambdaCatConf)
dparams = defaultParams
    { projectName = "lambdacat"
    , realMain    = mainCat
    , showError   = \ (_, c) s -> (Just s, c)
#ifdef DEBUG
    , ghcOpts     = ["-eventlog", "-threaded"]
#endif
    }
