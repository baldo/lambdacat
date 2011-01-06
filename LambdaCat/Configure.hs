module LambdaCat.Configure
    ( LambdaCatConf (..)
    , lambdaCatConf
    , setLCC
    ) where

import Data.IORef
import Foreign
import Network.URI

import LambdaCat.Internal.Class
    ( View (..)
    , Supplier (..)
    )

data LambdaCatConf = LambdaCatConf
    { supplierList :: [(Supplier, [Protocol])]
    , viewList     :: [(View    , [Protocol], [String])]
    , homeURI      :: URI
    , modifySupplierURI :: URI -> URI  
    , defaultURI   :: URI
    , defaultTitle :: String
    }

type Protocol = String

cfgIORef :: IORef LambdaCatConf
cfgIORef = unsafePerformIO $ newIORef (undefined :: LambdaCatConf)

lambdaCatConf :: LambdaCatConf
lambdaCatConf = unsafePerformIO $ readIORef cfgIORef


setLCC :: LambdaCatConf -> IO ()
setLCC = writeIORef cfgIORef
