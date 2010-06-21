{-# LANGUAGE MultiParamTypeClasses #-}

module LambdaCat.Uri
    ( Uri (..)
    )
where

import Control.Monad.Trans

class Error error

class MonadIO m => Uri m uri where
    toString :: uri -> m String
    fromString :: Error error => String -> m (Either uri error)

