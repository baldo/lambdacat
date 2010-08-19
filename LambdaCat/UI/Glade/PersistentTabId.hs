{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LambdaCat.UI.Glade.PersistentTabId 
    ( TabId 
    
    , genNewId
    , containerTabId
    ) where

import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import System.IO.Unsafe (unsafePerformIO)
import Graphics.UI.Gtk

newtype TabId = TabId Int
  deriving (Eq,Num,Show,Ord)

nextTabId :: MVar TabId
nextTabId = unsafePerformIO $ newMVar (TabId 0)

genNewId :: IO TabId
genNewId = modifyMVar nextTabId (\ t -> return (t + 1, t))

containerTabId :: Attr Widget (Maybe TabId)
containerTabId = unsafePerformIO $ objectCreateAttribute
