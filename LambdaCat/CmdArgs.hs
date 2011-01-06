{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module      : LambdaCat.CmdArgs
-- Copyright   : Andreas Baldeau, Daniel Ehlers
-- License     : BSD3
-- Maintainer  : Andreas Baldeau <andreas@baldeau.net>,
--               Daniel Ehlers <danielehlers@mindeye.net>
-- Stability   : Alpha
-- 
-- This module is used for command line parsing.

module LambdaCat.CmdArgs
    (
      -- * Command line argument representation
      CmdArgs

      -- * Selectors
    , recompile
    , uris

      -- * Retreiving command line arguments
    , getCmdArgs
    )
where

import Data.Version
import System.Environment

import System.Console.CmdArgs hiding
    ( CmdArgs
    , cmdArgs
    )
import qualified System.Console.CmdArgs as CA

import Paths_lambdacat

-- | CmdArgs stores the arguments given on the command line.
data CmdArgs = CmdArgs
    { recompile :: Bool      -- ^ If set, recompilation of user configuration
                             -- is forced.
    , ouris     :: [String]  -- ^ The URIs given by the @-u@ flags.
    , auris     :: [String]  -- ^ The URIs specified without any flag.
    -- , rts       :: String  -- For future use.
    , ignoreL   :: [String]  -- ^ This is used to allow correct parsing of
                             -- dyres command line arguments (please ignore).
    , ignoreB   :: Bool      -- ^ The same as 'ignoreL'.
    }
  deriving (Show, Eq, Data, Typeable)

-- | This value specifies how the command line arguments should be parsed by
-- the CmdArgs package and what help texts should be displayed.
cmdArgs :: CmdArgs
cmdArgs = CmdArgs
    { recompile = def
               &= explicit
               &= name "recompile"
               &= help "Recompile your config file and quit"

    , ouris     = def
               &= explicit
               &= name "u"
               &= name "uri"
               &= typ "URI"
               &= help "Load this URI (may be used more than once)"

    , auris     = def
               &= typ "URIS"
               &= args
{-
    , rts       = def
               &= explicit &= name "rts"
               &= (help $ "Specifies runtime system settings, "
                       ++ "e.g.: lambdacat --rts=\"-ls\"")
-}

    , ignoreL   = def
               &= explicit
               &= name "dyre-master-binary"
               &= help "For internal use only, should be hidden in the future"

    , ignoreB   = def
               &= explicit
               &= name "deny-reconf"
               &= help "For internal use only, should be hidden in the future"
    }
    &= summary ("lambdacat " ++ showVersion version)
    &= verbosity

-- | Get the String representations of the URIs given on command line.
uris :: CmdArgs -> [String]
uris ca = ouris ca ++ auris ca  -- TODO: Parse the URIs right here...

-- | Get the representation of the given command line arguments.
getCmdArgs :: IO CmdArgs
getCmdArgs = do
    _ <- getArgs -- TODO: Get rid of this dirty hack.
    CA.cmdArgs cmdArgs

