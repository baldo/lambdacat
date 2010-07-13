{-# LANGUAGE ExistentialQuantification #-}
module LambdaCat.Core 
    ( lambdaCatAddUI
    , lambdaCatURIChanged
    ) where

import Control.Concurrent.MVar
import System.IO.Unsafe
import Network.URI
import Control.Monad
import Control.Monad.Trans


import LambdaCat.Browser
import LambdaCat.UI
import LambdaCat.Page

data LambdaCatState m = LambdaCatState 
    { lcsBrowser :: [UI (Browser m) (Page m) m]
    }

mState = unsafePerformIO $ newMVar $ LambdaCatState 
        { lcsBrowser = [] 
        }

lambdaCatAddUI :: (MonadIO m, BrowserClass b m,PageClass p m) => UI (Browser m) (Page m)  m -> m ()
lambdaCatAddUI browser = liftIO $ do
    state <- takeMVar mState
    putMVar mState $ state { lcsBrowser = browser:(lcsBrowser state) }

lambdaCatURIChanged :: MonadIO m => Page m -> URI -> m ()
lambdaCatURIChanged page lambdaCatURI = do
    state <- liftIO $ readMVar mState
    mapM_ ( \ ui -> do 
        contains <- containsPage ui page
        when contains (uriChanged ui page)
        return ()
         ) $ lcsBrowser state
