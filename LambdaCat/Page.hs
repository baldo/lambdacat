{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, ExistentialQuantification, FlexibleInstances, RankNTypes #-}

module LambdaCat.Page
    ( UIClass (..)
    , PageClass (..)
    , Page (..)
    , HasWidget (..)

    , pageFromProtocol
    )
where

import LambdaCat.Protocol
import LambdaCat.Browser

import Data.Typeable
import Network.URI
import Graphics.UI.Gtk.Abstract.Widget

class UIClass ui where
    -- | Initializes the UI and returns an UI handle.
    init :: IO ui

    -- | Creates the main UI widget for the browser (e.g. a window).
    newBrowser :: ui -> IO BrowserId

    -- | Embed the page into the given browser.
    embedPage :: ui -> BrowserId -> Page -> IO ()

    replacePage :: ui -> BrowserId -> Page -> Page -> IO ()

    -- | Checks if a page is child of this brower/ui
    uriChanged   :: ui -> Page -> IO ()

    -- | Replace current title with the one from given page
    changedTitle :: ui -> Page -> IO ()

    update :: ui -> BrowserId -> CallBack ui 

    -- | The main loop for the UI.
    mainLoop :: ui -> IO ()

type CallBack ui = (ui -> BrowserId -> IO ()) -> IO ()

class Eq page => PageClass page where
    -- | Creates a new page.
    new :: UIClass ui => CallBack ui -> IO page

    -- | Some uri functions
    load :: page -> URI -> IO Bool

    -- |
    back, forward, stop, reload :: page -> IO ()
    back _ = return ()
    forward _ = return ()
    stop _ = return ()
    reload _ = return ()

    -- | generic informations on a page
    getCurrentURI :: page -> IO URI
    getCurrentTitle :: page -> IO String

    -- |
    getBackHistory, getForwardHistory :: page -> IO [URI]
    getBackHistory _ = return []
    getForwardHistory _ = return []

class WidgetClass w => HasWidget hw w | hw -> w where
    getWidget :: hw -> w

data Page = forall hw w . (Typeable hw, HasWidget hw w, PageClass hw) => Page hw

instance Eq Page where
    (Page p1) == (Page p2)
        | p1 `eqType` p2 = cast p1 == Just p2
        | otherwise      = False

instance PageClass Page where
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

eqPageType :: Page -> Page -> Bool
eqPageType (Page p1) (Page p2) = p1 `eqType` p2

createPage :: (PageClass p,UIClass ui) => p -> CallBack ui -> IO p
createPage _ = new

pageFromProtocol :: UIClass u => CallBack u -> [(Page, [Protocol])] -> Maybe Page -> Maybe URI -> IO (Maybe Page)
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

        lookupProtocol :: [(Page, [Protocol])] -> IO (Maybe Page)
        lookupProtocol [] = return Nothing
        lookupProtocol ((page, protos) : plist)
            | protocol `elem` protos = return $ Just page
            | otherwise              = lookupProtocol plist

