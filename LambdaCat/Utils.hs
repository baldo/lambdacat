module LambdaCat.Utils
    ( info
    , log
    )
where

import Prelude hiding (log)
import Control.Monad
import System.Console.CmdArgs.Verbosity

import Flags

unlessQuiet :: IO () -> IO ()
unlessQuiet f = do
    v <- getVerbosity
    when (v /= Quiet) f

info, log :: (a -> IO ()) -> a -> IO ()
info f | debug     = unlessQuiet . f
       | otherwise = whenNormal . f
log  f | debug     = unlessQuiet . f
       | otherwise = whenLoud . f

