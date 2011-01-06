module LambdaCat.Configure
    ( LambdaCatConf (..)
    , Protocol 

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

-- | lambdacat configuration datatype
data LambdaCatConf = LambdaCatConf
    { supplierList :: [(Supplier, [Protocol])] -- ^ list of suppliers with supported protocols 
    , viewList     :: [(View    , [Protocol], [String])] -- ^ list of 'View's with supported protocols
    , homeURI      :: URI -- ^ home uri 
    , modifySupplierURI :: URI -> URI  -- ^ supplier modifcator
    }

-- | Type for protocol. A protocol is a uri schema of the form "<name>:"
type Protocol = String

-- | magic configuration holder
--   this is not exported because it use the evil function
cfgIORef :: IORef LambdaCatConf
cfgIORef = unsafePerformIO $ newIORef (undefined :: LambdaCatConf)

-- | global configuration constant
lambdaCatConf :: LambdaCatConf
lambdaCatConf = unsafePerformIO $ readIORef cfgIORef

-- | set the lambdacat configuration
-- this function should be called only once by the lambdacat main
-- function and is only for internal use.
setLCC :: LambdaCatConf -> IO ()
setLCC = writeIORef cfgIORef
