{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

module LambdaCat.Page.MPlayer
    ( MPlayerPage
    ) where

import LambdaCat.Page hiding (Page)

import Prelude hiding (sin)
import Data.Typeable
import Graphics.UI.Gtk
import Control.Monad
import Control.Monad.Trans
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

data Handles = Handles
    { stdin  :: Handle
    , stdout :: Handle
    , stderr :: Handle
    }
  deriving Eq

instance HasWidget MPlayerPage Socket where
    getWidget = mplayerSocket

instance MonadIO m => PageClass MPlayerPage m where
    new _ = liftIO $ do
        socket   <- socketNew
        mHandles <- newMVar Nothing
        mURI     <- newMVar nullURI

        return $ MPlayerPage { mplayerSocket = socket, mplayerHandles = mHandles, mplayerURI = mURI }

    load page@MPlayerPage { mplayerSocket = socket } uri = liftIO $ do
        mplayerCommand page $ "quit"
        handles <- liftIO $ spawnMPlayer socket uri
        updateHandles page $ Just handles 
        return True

    getCurrentURI = flip withURI return
    getCurrentTitle = flip withURI (return . (flip (uriToString id) ""))

withHandles :: MonadIO m => MPlayerPage -> (Handles -> m ()) -> m ()
withHandles MPlayerPage { mplayerHandles = mHandles } f = do
    mh <- liftIO $ takeMVar mHandles
    maybe (return ()) f mh
    liftIO $ putMVar mHandles (mh)

updateHandles :: MonadIO m => MPlayerPage -> Maybe Handles -> m ()
updateHandles MPlayerPage { mplayerHandles = mHandles } mh = liftIO $ modifyMVar_ mHandles (\_ -> return mh)

withURI :: MonadIO m => MPlayerPage -> (URI -> m a) -> m a
withURI MPlayerPage { mplayerURI = mURI } f = do
    uri <- liftIO $ takeMVar mURI
    r <- f uri
    liftIO $ putMVar mURI uri 
    return r

toHandles :: (Handle, Handle, Handle, a) -> Handles
toHandles (sin, sout, serr, _) = Handles { stdin = sin, stdout = sout, stderr = serr }

spawnMPlayer :: Socket -> URI -> IO Handles
spawnMPlayer socket uri = do
    widgetShowAll socket
    wid <- liftM fromNativeWindowId $ socketGetId socket
    let commandLine = "mplayer " ++ uriToString id uri "" ++ " -gui -idle -slave -wid " ++ show (wid :: Int)
    print commandLine
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
    hGetLine (f handles) >>= putStrLn
    monitor handles f
