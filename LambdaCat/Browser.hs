{-# LANGUAGE MultiParamTypeClasses, ExistentialQuantification, FlexibleInstances, GeneralizedNewtypeDeriving #-}

module LambdaCat.Browser
    ( BrowserId

    , newBrowserId
    )
where

import Control.Concurrent.MVar
import System.IO.Unsafe

newtype BrowserId = BrowserId Int
    deriving (Eq, Show, Ord, Num)

nextId :: MVar BrowserId
nextId = unsafePerformIO $ newMVar (BrowserId 0)

newBrowserId :: IO BrowserId
newBrowserId = do
    i <- takeMVar nextId
    putMVar nextId (i + 1)
    return i
