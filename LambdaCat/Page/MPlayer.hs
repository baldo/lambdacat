{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

module LambdaCat.Page.MPlayer
    ( MPlayerPage

    , mplayerPage
    ) where

import LambdaCat.Page
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
    , mplayerHandles :: MVar (Maybe Handles)
    , mplayerURI     :: MVar URI
    }
  deriving (Eq, Typeable)

mplayerPage :: Page
mplayerPage = Page (undefined :: MPlayerPage)

data Handles = Handles
    { stdin  :: Handle
    , stdout :: Handle
    , stderr :: Handle
    }
  deriving Eq

instance HasWidget MPlayerPage Socket where
    getWidget = mplayerSocket

instance PageClass MPlayerPage where
    new _ = do
        socket   <- socketNew
        mHandles <- newMVar Nothing
        mURI     <- newMVar nullURI

        return $ MPlayerPage { mplayerSocket = socket, mplayerHandles = mHandles, mplayerURI = mURI }
    
    destroy _ = return ()

    load page@MPlayerPage { mplayerSocket = socket } uri = do
        mplayerCommand page $ "quit"
        handles <- spawnMPlayer socket uri
        updateHandles page $ Just handles
        return True

    getCurrentURI = flip withURI return
    getCurrentTitle = flip withURI (return . (flip (uriToString id) ""))

withHandles :: MPlayerPage -> (Handles -> IO ()) -> IO ()
withHandles MPlayerPage { mplayerHandles = mHandles } f = do
    mh <- takeMVar mHandles
    maybe (return ()) f mh
    putMVar mHandles (mh)

updateHandles :: MPlayerPage -> Maybe Handles -> IO ()
updateHandles MPlayerPage { mplayerHandles = mHandles } mh = modifyMVar_ mHandles (\_ -> return mh)

withURI :: MPlayerPage -> (URI -> IO a) -> IO a
withURI MPlayerPage { mplayerURI = mURI } f = do
    uri <- takeMVar mURI
    r <- f uri
    putMVar mURI uri
    return r

toHandles :: (Handle, Handle, Handle, a) -> Handles
toHandles (sin, sout, serr, _) = Handles { stdin = sin, stdout = sout, stderr = serr }

spawnMPlayer :: Socket -> URI -> IO Handles
spawnMPlayer socket uri = do
    widgetShowAll socket
    wid <- liftM fromNativeWindowId $ socketGetId socket
    let commandLine = "mplayer " ++ uriToString id uri "" ++ " -gui -idle -slave -wid " ++ show (wid :: Int)
    log print commandLine
    tHandles <- runInteractiveCommand commandLine
    let handles = toHandles tHandles
    _ <- forkIO $ monitor handles stdout
    _ <- forkIO $ monitor handles stderr
    return handles

mplayerCommand :: MPlayerPage -> String -> IO ()
mplayerCommand page cmd = do
    withHandles page $ \handles -> do
        let sin = stdin handles
        hPutStrLn sin (cmd ++ "\n")
    return ()

monitor :: Handles -> (Handles -> Handle) -> IO ()
monitor handles f = do
    hGetLine (f handles) >>= log putStrLn
    monitor handles f
