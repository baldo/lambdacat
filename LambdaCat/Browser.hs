{-# LANGUAGE MultiParamTypeClasses, ExistentialQuantification, FlexibleInstances #-}

module LambdaCat.Browser
    ( Browser (..)
    , BrowserClass (..)
    )
where

import Control.Monad.Trans

class MonadIO m => BrowserClass browser m

data Browser m = forall b . BrowserClass b m => Browser b

instance MonadIO m => BrowserClass (Browser m) m where

