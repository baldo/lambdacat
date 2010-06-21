{-# LANGUAGE MultiParamTypeClasses #-}

module LambdaCat.Page
    ( Page (..)
    )
where

import LambdaCat.Uri

import Control.Monad.Trans

class MonadIO m => Page m page where
    -- | Creates a new page.
    new :: m page

    -- | 
    back, forward, stop, reload :: page -> m ()
    back _ = return ()
    forward _ = return ()
    stop _ = return ()
    reload _ = return ()

    -- |
    getBackHistory, getForwardHistory :: Uri m u => page -> m [u]
    getBackHistory _ = return []
    getForwardHistory _ = return []

