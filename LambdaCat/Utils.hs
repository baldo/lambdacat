{-# LANGUAGE TemplateHaskell #-}

module LambdaCat.Utils
    ( info
    , log

    , pinfo
    , plog
    )
where

import Prelude hiding (log)
import Control.Monad
import Language.Haskell.TH
import System.Console.CmdArgs.Verbosity

import Flags

unlessQuiet :: IO () -> IO ()
unlessQuiet f = do
    v <- getVerbosity
    when (v /= Quiet) f

$( if debug
       then [d| info :: (a -> IO ()) -> a -> IO ()
                info f = unlessQuiet . f

                log :: (a -> IO ()) -> a -> IO ()
                log f = unlessQuiet . f
            |]

       else [d| info :: (a -> IO ()) -> a -> IO ()
                info f = whenNormal . f

                log :: (a -> IO ()) -> a -> IO ()
                log f = whenLoud . f
            |]
 )

pinfo :: Q Exp
pinfo = do
    loc <- location
    let fn     = loc_filename loc
        (l, c) = loc_start loc
        pos    = fn ++ ":" ++ show l ++ ":" ++ show c ++ ": "
    if debug
        then [| \f x -> putStr pos >> info f x |]
        else [| info |]

plog :: Q Exp
plog = do
    loc <- location
    let fn     = loc_filename loc
        (l, c) = loc_start loc
        pos    = fn ++ ":" ++ show l ++ ":" ++ show c ++ ": "
    if debug
        then [| \f x -> putStr pos >> log f x |]
        else [| log |]

