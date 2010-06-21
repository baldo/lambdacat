{-# LANGUAGE MultiParamTypeClasses #-}

module LambdaCat.Loadable
    ( Loadable (..)
    )
where

import LambdaCat.Page

import Control.Monad.Trans

class MonadIO m => Loadable m l where
    load :: Page m page => page -> l -> m ()

