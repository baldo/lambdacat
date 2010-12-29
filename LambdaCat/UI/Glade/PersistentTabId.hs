{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LambdaCat.UI.Glade.PersistentTabId
    ( TabId

    , genNewId
    , containerTabId
    , withContainerId
    , withUnsafeContainerId 
    , setContainerId
    ) where

import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import System.IO.Unsafe (unsafePerformIO)
import Graphics.UI.Gtk
import Data.Maybe (fromJust)

newtype TabId = TabId Int
  deriving (Eq, Num, Show, Ord)

nextTabId :: MVar TabId
nextTabId = unsafePerformIO $ newMVar (TabId 0)

genNewId :: IO TabId
genNewId = modifyMVar nextTabId (\ t -> return (t + 1, t))

containerTabId :: Attr Container (Maybe TabId)
containerTabId = unsafePerformIO objectCreateAttribute

setContainerId
    :: ContainerClass container
    => container
    -> TabId
    -> IO ()
setContainerId container tabId =
    set (castToContainer container) [ containerTabId := Just tabId ]

withContainerId
    :: (ContainerClass container)
    => container
    -> (TabId -> IO ())
    -> IO ()
withContainerId container f = do
 maybeId <- get (castToContainer container) containerTabId
 case maybeId of
    Just cId -> f cId
    Nothing  -> return ()

withUnsafeContainerId
    :: (ContainerClass container)
    => container
    -> (TabId -> IO a)
    -> IO a
withUnsafeContainerId container f = do
  get (castToContainer container) containerTabId >>= f . fromJust 
