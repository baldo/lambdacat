{-# LANGUAGE MultiParamTypeClasses, ExistentialQuantification, FlexibleInstances, GeneralizedNewtypeDeriving #-}

module LambdaCat.Browser
    ( BrowserID
    , newBrowserID
    )
where

import Control.Monad.Trans
import Control.Concurrent.MVar
import System.IO.Unsafe

newtype BrowserID = BrowserID Int
    deriving (Eq,Show,Ord,Num)

nextID :: MVar BrowserID
nextID = unsafePerformIO $ newMVar (BrowserID 0)

newBrowserID :: MonadIO m => m BrowserID
newBrowserID = liftIO $ do 
    i <- takeMVar nextID
    putMVar nextID (i + 1)
    return i 
