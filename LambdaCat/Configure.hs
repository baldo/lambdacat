module LambdaCat.Configure 
    ( LambdaCatConf (..)
    , lambdaCatConf
    , setLCC
    ) where

import Data.IORef
import Foreign
import LambdaCat.Page (Page (..))
import LambdaCat.Protocol 

data LambdaCatConf = LambdaCatConf 
    { pageList :: [(Page,[Protocol])]
    , mimeList :: [(Page,[String])]
    }

cfgIORef :: IORef LambdaCatConf
cfgIORef = unsafePerformIO $ newIORef (undefined :: LambdaCatConf)

lambdaCatConf :: LambdaCatConf
lambdaCatConf = unsafePerformIO $ readIORef cfgIORef

setLCC :: LambdaCatConf -> IO ()
setLCC conf = writeIORef cfgIORef conf
