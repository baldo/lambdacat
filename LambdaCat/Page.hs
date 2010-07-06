{-# LANGUAGE MultiParamTypeClasses, ExistentialQuantification, FlexibleInstances #-}

module LambdaCat.Page
    ( Page (..)
    , PageClass (..)
    )
where

import LambdaCat.Protocol

import Control.Monad.Trans
import Network.URI

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
    getBackHistory, getForwardHistory :: page -> m [URI]
    getBackHistory _ = return []
    getForwardHistory _ = return []

data Page m = forall a . (PageClass a m) => Page a 

instance MonadIO m => PageClass (Page m) m where
   new = return (error "Can't create existential quantificated datatype")  
   
   back (Page p) = back p
   forward (Page p) = forward p
   stop (Page p) = stop p
   reload (Page p) = reload p
   
   getBackHistory (Page p) = getBackHistory p
   getForwardHistory (Page p) = getForwardHistory p
