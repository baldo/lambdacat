{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : LambdaCat.UI.Glade.PersistentTabId
-- Copyright   : Andreas Baldeau, Daniel Ehlers
-- License     : BSD3
-- Maintainer  : Andreas Baldeau <andreas@baldeau.net>,
--               Daniel Ehlers <danielehlers@mindeye.net>
-- Stability   : Alpha
--
-- This module provides tab identifiers that are globally unique.

module LambdaCat.UI.Glade.PersistentTabId
    (
      -- * Datatype
      TabId

      -- * Container attribute
    , containerTabId
    , setContainerId

      -- * Construction
    , genNewId

      -- * Access
    , withContainerId
    , withUnsafeContainerId
    )
where

import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , newMVar
    )
import Data.Maybe
    ( fromJust
    )
import System.IO.Unsafe
    ( unsafePerformIO
    )

import Graphics.UI.Gtk

-- | The type for unique tab identifiers.
newtype TabId = TabId Int
  deriving (Eq, Num, Show, Ord)

-- | MVar holding the next identifier.
nextTabId :: MVar TabId
nextTabId = unsafePerformIO $ newMVar (TabId 0)

-- | Generate a new unique identifier.
genNewId :: IO TabId
genNewId = modifyMVar nextTabId $ \t -> return (t + 1, t)

-- | Attribute to store a unique tab identifier in a container.
containerTabId :: Attr Container (Maybe TabId)
containerTabId = unsafePerformIO objectCreateAttribute

-- | Set a new identifier in the given container.
--
-- Same as @set container [ containerTabId := tabId ]@.
setContainerId
    :: ContainerClass container
    => container  -- ^ The container to set the identifier in.
    -> TabId      -- ^ The new tab identifier.
    -> IO ()
setContainerId container tabId =
    set (castToContainer container) [ containerTabId := Just tabId ]

-- | Call the given function with the tab identifier stored in the container.
-- If no identifier is stored the function will return.
withContainerId
    :: (ContainerClass container)
    => container
    -> (TabId -> IO ())
    -> IO ()
withContainerId container f = do
    maybeId <- get (castToContainer container) containerTabId

    case maybeId of
        Just cId ->
            f cId

        Nothing ->
            return ()

-- | Call the given function with the tab identifier stored in the container.
-- If no identifier is stored the function will raise an exception.
withUnsafeContainerId
    :: (ContainerClass container)
    => container
    -> (TabId -> IO a)
    -> IO a
withUnsafeContainerId container f = do
    mTabId <- get (castToContainer container) containerTabId
    f $ fromJust mTabId

