{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, ExistentialQuantification, FlexibleInstances, RankNTypes #-}

module LambdaCat.Page
    ( UIClass (..)
    , PageClass (..)
    , Page (..)
    , HasWidget (..)
    , SinkMonad (..)

    , pageFromProtocol
    )
where

import LambdaCat.Protocol
import LambdaCat.Browser

import Control.Monad.Trans
import Data.Typeable
import Network.URI
import Graphics.UI.Gtk.Abstract.Widget

class MonadIO m => UIClass ui m where
    -- | Initializes the UI and returns an UI handle.
    init :: m ui

    -- | Creates the main UI widget for the browser (e.g. a window).
    newBrowser :: ui -> m BrowserID

    -- | Embed the page into the given browser.
    embedPage :: ui -> BrowserID -> Page m -> m ()

    replacePage :: ui -> BrowserID -> Page m -> Page m -> m ()

    -- | Checks if a page is child of this brower/ui
    uriChanged   :: ui -> Page m -> m ()

    -- | Replace current title with the one from given page
    changedTitle :: ui -> Page m -> m ()

    update :: ui -> CallBack ui m 

    -- | The main loop for the UI.
    mainLoop :: ui -> m ()

type CallBack ui m = (ui -> m ()) -> m ()

class (Eq page, MonadIO m) => PageClass page m where
    -- | Creates a new page.
    new :: (UIClass ui m) => CallBack ui m -> m page

    -- | Some uri functions
    load :: page -> URI -> m Bool

    -- |
    back, forward, stop, reload :: page -> m ()
    back _ = return ()
    forward _ = return ()
    stop _ = return ()
    reload _ = return ()

    -- | generic informations on a page
    getCurrentURI :: page -> m URI
    getCurrentTitle :: page -> m String

    -- |
    getBackHistory, getForwardHistory :: page -> m [URI]
    getBackHistory _ = return []
    getForwardHistory _ = return []

class WidgetClass w => HasWidget hw w | hw -> w where
    getWidget :: hw -> w

class MonadIO m => SinkMonad m where
    getSink :: (MonadIO m') => m (m a -> m' a)

data Page m = forall hw w . (Typeable hw, HasWidget hw w, PageClass hw m) => Page hw

instance MonadIO m => Eq (Page m) where
    (Page p1) == (Page p2)
        | p1 `eqType` p2 = cast p1 == Just p2
        | otherwise      = False

instance MonadIO m => PageClass (Page m) m where
    new = return (error "Can't create existential quantificated datatype")

    load (Page p) = load p

    back (Page p) = back p
    forward (Page p) = forward p
    stop (Page p) = stop p
    reload (Page p) = reload p

    getCurrentURI (Page p)   = getCurrentURI p 
    getCurrentTitle (Page p) = getCurrentTitle p

    getBackHistory (Page p) = getBackHistory p
    getForwardHistory (Page p) = getForwardHistory p

eqType :: (Typeable a, Typeable b) => a -> b -> Bool
eqType a b = typeOf a == typeOf b

eqPageType :: Page m1 -> Page m2 -> Bool
eqPageType (Page p1) (Page p2) = p1 `eqType` p2

createPage :: (MonadIO m, PageClass p m,UIClass ui m) => p -> CallBack ui m -> m p
createPage _ = new

pageFromProtocol :: (MonadIO m,UIClass u m) => CallBack u m -> [(Page m, [Protocol])] -> Maybe (Page m) -> Maybe URI -> m (Maybe (Page m))
pageFromProtocol _  _  _  Nothing    = return Nothing
pageFromProtocol _  [] _  _          = return Nothing
pageFromProtocol cb ps mp (Just uri) = do
    mp' <- lookupProtocol ps

    case (mp, mp') of
        (_,       Nothing       ) -> return Nothing
        (Nothing, Just (Page p')) -> createPage p' cb >>= return . Just . Page
        (Just (Page p), Just (Page p'))
            | p `eqType` p' -> return mp
            | otherwise     -> createPage p' cb >>= return . Just . Page

    where
        protocol = uriScheme uri

        lookupProtocol :: MonadIO m => [(Page m, [Protocol])] -> m (Maybe (Page m))
        lookupProtocol [] = return Nothing
        lookupProtocol ((page, protos) : plist)
            | protocol `elem` protos = return $ Just page
            | otherwise              = lookupProtocol plist

