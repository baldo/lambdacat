{-# LANGUAGE DeriveDataTypeable #-}

module LambdaCat.CmdArgs
    ( CmdArgs
    , recompile
    , uris

    , getCmdArgs
    )
where

import Paths_lambdacat

import Data.Version
import System.Console.CmdArgs hiding (cmdArgs, CmdArgs)
import qualified System.Console.CmdArgs as CA

import System.Environment

data CmdArgs = CmdArgs
    { recompile :: Bool
    , ouris     :: [String]
    , auris     :: [String]
    -- , rts       :: String -- For future use.
    , ignoreL   :: [String]
    , ignoreB   :: Bool
    }
  deriving (Show, Eq, Data, Typeable)

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

uris :: CmdArgs -> [String]
uris ca = ouris ca ++ auris ca

getCmdArgs :: IO CmdArgs
getCmdArgs = do
    _ <- getArgs -- TODO: Get rid of this dirty hack.
    CA.cmdArgs cmdArgs

