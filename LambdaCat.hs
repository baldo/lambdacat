module LambdaCat
    ( lambdacat
    , defaultConfig
    , LambdaCatConf (..)
    )
where

import LambdaCat.Configure
import LambdaCat.Supply
import LambdaCat.CmdArgs
import LambdaCat.View.Web (WebView)
import LambdaCat.UI.Glade 
import LambdaCat.Class as Class

import Config.Dyre
import Config.Dyre.Compile
import Data.Maybe
import Network.URI
import System.Exit
import System.IO

defaultConfig :: LambdaCatConf
defaultConfig = LambdaCatConf
    { supplierList = [ (webSupplier   , ["http:","https:","about:"])
                     ]
    , viewList     = [ (View (undefined :: WebView) , ["about:","http:", "https:", "file:"], [])
                     ]
    , homeURI      = fromJust $ parseURI "http://www.haskell.org"
    }

mainCat :: (Maybe String, LambdaCatConf) -> IO ()
mainCat (e, cfg) = do
    maybe (return ()) error e

    setLCC cfg
    args <- getCmdArgs

    let us = if null $ uris args
             then [show $ homeURI cfg]
             else uris args
    ui <- Class.init :: IO GladeUI
    mapM_ (supplyForView (Class.update ui undefined) embedView . fromJust . parseURIReference) us
    mainLoop ui

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
dparams =
    let dps = defaultParams
            { projectName = "lambdacat"
            , realMain    = mainCat
            , showError   = \ (_, c) s -> (Just s, c)
            , statusOut   = putStrLn
            }
    in  dps { ghcOpts = ["-eventlog", "-threaded"] }
