module LambdaCat.Configure
    ( LambdaCatConf (..)
    , lambdaCatConf
    , setLCC
    ) where

import Data.IORef
import Foreign
import Network.URI

import LambdaCat.Page (Page (..))
import LambdaCat.Protocol

data LambdaCatConf = LambdaCatConf
    { uriModifier :: URI -> URI
    , pageList    :: [(Page, [Protocol])]
    , mimeList    :: [(Page, [String])]
    , homeURI     :: URI
    }

cfgIORef :: IORef LambdaCatConf
cfgIORef = unsafePerformIO $ newIORef (undefined :: LambdaCatConf)

lambdaCatConf :: LambdaCatConf
lambdaCatConf = unsafePerformIO $ readIORef cfgIORef

setLCC :: LambdaCatConf -> IO ()
setLCC conf = writeIORef cfgIORef conf
