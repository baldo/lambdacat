{-# LANGUAGE MultiParamTypeClasses #-}

module LambdaCat.Loadable
    ( Loadable (..)
    )
where

import LambdaCat.Page

import Control.Monad.Trans

class MonadIO m => Loadable l m where
    load :: PageClass page m => page -> l -> m ()

