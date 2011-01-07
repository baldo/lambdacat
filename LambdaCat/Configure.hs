-- |
-- Module      : LambdaCat.Configure
-- Copyright   : Andreas Baldeau, Daniel Ehlers
-- License     : BSD3
-- Maintainer  : Andreas Baldeau <andreas@baldeau.net>,
--               Daniel Ehlers <danielehlers@mindeye.net>
-- Stability   : Alpha
--
-- This module provides the data structure 'LambdaCatConf' which holds
-- lambdacats global configuration. The configuration can easily accessed
-- through 'lambdaCatConf'.

module LambdaCat.Configure
    (
      -- * The configuration data structure
      LambdaCatConf (..)
    , Protocol

      -- * Global access
    , lambdaCatConf
    , setLCC
    )
where

import Data.IORef
import Network.URI
import System.IO.Unsafe

import LambdaCat.Internal.Class
    ( View (..)
    , Supplier (..)
    )

-- | Lambdacat's configuration datatype.
data LambdaCatConf = LambdaCatConf
    { supplierList
        :: [(Supplier, [Protocol])]        -- ^ Suppliers with supported
                                           -- protocols.
    , viewList
        :: [(View, [Protocol], [String])]  -- ^ Views with supported
                                           -- protocols.
    , homeURI
        :: URI                             -- ^ URI of the home page.
    , modifySupplierURI
        :: URI -> URI                      -- ^ Function to modify the URIs
                                           -- before they are given to the
                                           -- Supplier.
    , defaultURI
        :: URI                             -- ^ Default URI for e.g. a
                                           -- new tab.
    , defaultTitle
        :: String                          -- ^ Default title for views that
                                           -- don't (yet) have a title.
    }

-- | Type for protocols. A protocol is a uri schema of the form "<name>:"
type Protocol = String

-- | This IORef holds the global configuration. It can be accessed through
-- 'lambdaCatConf'.
cfgIORef :: IORef LambdaCatConf
cfgIORef = unsafePerformIO $ newIORef (undefined :: LambdaCatConf)

-- | Global value that provides access to the global configuration.
lambdaCatConf :: LambdaCatConf
lambdaCatConf = unsafePerformIO $ readIORef cfgIORef

-- | Sets the lambdacat configuration.
--
-- This function should be called only once by the lambdacat main
-- function and is only for internal use.
setLCC :: LambdaCatConf -> IO ()
setLCC = writeIORef cfgIORef

