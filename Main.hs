-- | This is the main module for lambdacat's global binary. Its purpose is
-- (re-)compiling the config file (real main module) and running the
-- generated binary.

module Main
    ( main
    )
where

import LambdaCat

main :: IO ()
main = lambdacat defaultConfig
