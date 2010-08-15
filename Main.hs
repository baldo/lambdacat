module Main 
    ( main
    ) where

import LambdaCat
import GHC.Paths ( ghc )

main = do 
    putStrLn ghc
    lambdacat defaultConfig
