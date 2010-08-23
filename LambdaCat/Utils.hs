{-# LANGUAGE CPP #-}

module LambdaCat.Utils
    ( info
    , log
    )
where

import Prelude hiding (log)

#ifndef DEBUG
import System.Console.CmdArgs.Verbosity
#endif

info, log :: (a -> IO ()) -> a -> IO ()
#ifdef DEBUG
info = id
log  = id
#else
info f = whenNormal . f
log  f = whenLoud   . f
#endif
