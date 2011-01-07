-- |
-- Module      : Main
-- Copyright   : Andreas Baldeau, Daniel Ehlers
-- License     : BSD3
-- Maintainer  : Andreas Baldeau <andreas@baldeau.net>,
--               Daniel Ehlers <danielehlers@mindeye.net>
-- Stability   : Alpha
--
-- This is the main module for lambdacat's global binary. Its purpose is
-- (re-)compiling the config file (real main module) and running the
-- generated binary.

module Main
    ( main
    )
where

import LambdaCat

main :: IO ()
main = lambdacat defaultConfig
