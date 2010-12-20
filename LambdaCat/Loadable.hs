{-# LANGUAGE MultiParamTypeClasses #-}

module LambdaCat.Loadable
    ( Loadable (..)
    )
where

import LambdaCat.Class

class Loadable l where
    load :: ViewClass view => view -> l -> IO ()

