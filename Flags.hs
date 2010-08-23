{-# LANGUAGE CPP #-}

module Flags where

debug :: Bool
#ifdef DEBUG
debug = True
#else
debug = False
#endif

