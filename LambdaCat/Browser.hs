{-# LANGUAGE MultiParamTypeClasses, ExistentialQuantification #-}

module LambdaCat.Browser
    ( Browser (..)
    , BrowserClass (..)
    )
where

import Control.Monad.Trans

data Browser m = forall b . BrowserClass b m => Browser b

class MonadIO m => BrowserClass browser m

