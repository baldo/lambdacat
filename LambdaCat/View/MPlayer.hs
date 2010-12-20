{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}

module LambdaCat.View.MPlayer
    ( MPlayerPage

    , mplayerPage
    ) where

import LambdaCat.View
import LambdaCat.Utils

import Prelude hiding (log, sin)
import Data.Typeable
import Graphics.UI.Gtk
import Control.Monad
import Network.URI
import Control.Concurrent
import System.IO hiding (stdin, stdout, stderr)
import System.Process

data MPlayerPage = MPlayerPage
    { mplayerSocket  :: Socket
    , mplayerWidget  :: ScrolledWindow
    , mplayerHandles :: MVar (Maybe Handles)
    , mplayerURI     :: MVar URI
    }
  deriving (Eq, Typeable)

mplayerPage :: View
mplayerPage = View (undefined :: MPlayerPage)

data Handles = Handles
    { stdin  :: Handle
    , stdout :: Handle
    , stderr :: Handle
    }
  deriving Eq

instance HasWidget MPlayerPage ScrolledWindow where
    getWidget = mplayerWidget

instance PageClass MPlayerPage where
    new _ = do
        swrapper <- scrolledWindowNew Nothing Nothing

        set swrapper [ scrolledWindowHscrollbarPolicy := PolicyNever
                     , scrolledWindowVscrollbarPolicy := PolicyNever
                     ]

        socket   <- socketNew
        mHandles <- newMVar Nothing
        mURI     <- newMVar nullURI

        let page = MPlayerPage
                    { mplayerSocket  = socket
                    , mplayerWidget  = swrapper
                    , mplayerHandles = mHandles
                    , mplayerURI     = mURI
                    }

        return page

    destroy page = mplayerCommand page "quit"

    load page uri = do
        mplayerCommand page $ "loadfile " ++ show uri ++ " 0"
        return True

    getCurrentURI = flip withURI return
    getCurrentTitle = flip withURI (return . flip (uriToString id) "")

withHandles :: MPlayerPage -> (Handles -> IO ()) -> IO ()
withHandles MPlayerPage
                { mplayerWidget  = swrapper
                , mplayerSocket  = socket
                , mplayerHandles = mHandles
                }
            f = do
    mh <- takeMVar mHandles
    h  <- maybe ( do
                    scrolledWindowAddWithViewport swrapper socket
                    spawnMPlayer socket
                ) return mh

    f h
    putMVar mHandles $ Just h

--updateHandles :: MPlayerPage -> Maybe Handles -> IO ()
--updateHandles MPlayerPage { mplayerHandles = mHandles } mh = modifyMVar_ mHandles (\ _ -> return mh)

withURI :: MPlayerPage -> (URI -> IO a) -> IO a
withURI MPlayerPage { mplayerURI = mURI } f = do
    uri <- takeMVar mURI
    r <- f uri
    putMVar mURI uri
    return r

toHandles :: (Handle, Handle, Handle, a) -> Handles
toHandles (sin, sout, serr, _) = Handles { stdin = sin, stdout = sout, stderr = serr }

spawnMPlayer :: Socket -> IO Handles
spawnMPlayer socket = do
    widgetShowAll socket
    wid <- liftM fromNativeWindowId $ socketGetId socket
    let commandLine = "mplayer -gui -noconsolecontrols -idle -slave -wid " ++ show (wid :: Int)
    $plog print commandLine
    tHandles <- runInteractiveCommand commandLine
    let handles = toHandles tHandles
    _ <- forkIO $ monitor handles stdout
    _ <- forkIO $ monitor handles stderr
    return handles

mplayerCommand :: MPlayerPage -> String -> IO ()
mplayerCommand page cmd = do
    withHandles page $ \ handles -> do
        let sin = stdin handles
        hPutStrLn sin (cmd ++ "\n")
        hFlush sin
    return ()

monitor :: Handles -> (Handles -> Handle) -> IO ()
monitor handles f = do
    hGetLine (f handles) >>= $plog putStrLn
    monitor handles f
