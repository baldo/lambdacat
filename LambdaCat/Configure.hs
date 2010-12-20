module LambdaCat.Configure
    ( LambdaCatConf (..)
    , lambdaCatConf
    , setLCC
    ) where

import Data.IORef
import Foreign
import Network.URI

import LambdaCat.Class (View (..),Supplier(..))

data LambdaCatConf = LambdaCatConf
    { supplierList :: [(Supplier, [Protocol])]
    , viewList     :: [(View    , [Protocol], [String])]
    , homeURI      :: URI
    }

type Protocol = String

cfgIORef :: IORef LambdaCatConf
cfgIORef = unsafePerformIO $ newIORef (undefined :: LambdaCatConf)

lambdaCatConf :: LambdaCatConf
lambdaCatConf = unsafePerformIO $ readIORef cfgIORef


setLCC :: LambdaCatConf -> IO ()
setLCC = writeIORef cfgIORef
