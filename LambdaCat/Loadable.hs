{-# LANGUAGE MultiParamTypeClasses #-}

module LambdaCat.Loadable
    ( Loadable (..)
    )
where

import LambdaCat.Page

class Loadable l where
    load :: PageClass page => page -> l -> IO ()

