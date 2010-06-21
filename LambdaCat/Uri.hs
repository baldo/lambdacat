{-# LANGUAGE MultiParamTypeClasses #-}

module LambdaCat.Uri
    ( Uri (..)
    )
where

import Control.Monad.Trans

class Error error

class MonadIO m => Uri uri m where
    toString :: uri -> m String
    fromString :: Error error => String -> m (Either uri error)

