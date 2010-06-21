{-# LANGUAGE MultiParamTypeClasses #-}

module LambdaCat.Browser
    ( Browser (..)
    )
where

import Control.Monad.Trans

class MonadIO m => Browser browser m

