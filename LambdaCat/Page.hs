{-# LANGUAGE MultiParamTypeClasses, ExistentialQuantification #-}

module LambdaCat.Page
    ( Page (..)
    , PageClass (..)
    )
where

import LambdaCat.Uri

import Control.Monad.Trans

data Page m = forall a . (PageClass a m) => Page a 

class MonadIO m => PageClass page m where
    -- | Creates a new page.
    new :: m page

    -- | 
    back, forward, stop, reload :: page -> m ()
    back _ = return ()
    forward _ = return ()
    stop _ = return ()
    reload _ = return ()

    -- |
    getBackHistory, getForwardHistory :: Uri u m => page -> m [u]
    getBackHistory _ = return []
    getForwardHistory _ = return []

