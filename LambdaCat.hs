module LambdaCat
    ( lambdacat
    , defaultConfig
    , defaultModifySupplierURI
    , LambdaCatConf (..)
    )
where

import LambdaCat.Configure
import LambdaCat.Supplier
import LambdaCat.Supplier.Web
import LambdaCat.CmdArgs
import LambdaCat.View.Web (webView)
import LambdaCat.UI.Glade as UI

import Config.Dyre
import Config.Dyre.Compile
import Data.Maybe
import Network.URI
import System.Exit
import System.IO

defaultConfig :: LambdaCatConf
defaultConfig = LambdaCatConf
    { modifySupplierURI = defaultModifySupplierURI
    , supplierList = [ (webSupplier   , ["http:","https:","about:"])
                     ]
    , viewList     = [ (webView, ["about:","http:", "https:", "file:"], [])
                     ]
    , homeURI      = fromJust $ parseURI "http://www.haskell.org"
    }

defaultModifySupplierURI :: URI -> URI
defaultModifySupplierURI = id

mainCat :: (Maybe String, LambdaCatConf) -> IO ()
mainCat (e, cfg) = do
    maybe (return ()) error e

    setLCC cfg
    args <- getCmdArgs

    let  uria = map parseURIReference $ uris args
         us   = if null uria
                then [Just $ homeURI cfg]
                else uria
    ui <- UI.init :: IO GladeUI
    mapM_ (supplyForView (UI.update ui undefined) embedView . fromJust) us
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
