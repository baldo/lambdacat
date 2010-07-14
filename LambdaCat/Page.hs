{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, ExistentialQuantification, FlexibleInstances #-}

module LambdaCat.Page
    ( Page (..)
    , PageClass (..)
    , HasWidget (..)
    , pageFromProtocol
    )
where

import LambdaCat.Protocol

import Control.Monad.Trans
import Data.Typeable
import Network.URI
import Graphics.UI.Gtk.Abstract.Widget

class MonadIO m => PageClass page m where
    -- | Creates a new page.
    new :: (Page m -> m ()) -> m page

    -- | Some uri functions
    load :: page -> URI -> m ()

    -- |
    back, forward, stop, reload :: page -> m ()
    back _ = return ()
    forward _ = return ()
    stop _ = return ()
    reload _ = return ()

    -- |
    getBackHistory, getForwardHistory :: page -> m [URI]
    getBackHistory _ = return []
    getForwardHistory _ = return []

class WidgetClass w => HasWidget hw w | hw -> w where
    getWidget :: hw -> w

data Page m = forall hw w . (Typeable hw, HasWidget hw w, PageClass hw m) => Page hw

instance MonadIO m => PageClass (Page m) m where
   new = return (error "Can't create existential quantificated datatype")

   load (Page p) = load p

   back (Page p) = back p
   forward (Page p) = forward p
   stop (Page p) = stop p
   reload (Page p) = reload p

   getBackHistory (Page p) = getBackHistory p
   getForwardHistory (Page p) = getForwardHistory p

eqType :: (Typeable a, Typeable b) => a -> b -> Bool
eqType a b = typeOf a == typeOf b

eqPageType :: Page m1 -> Page m2 -> Bool
eqPageType (Page p1) (Page p2) = p1 `eqType` p2

createPage :: (MonadIO m, PageClass p m) => p -> (Page m -> m ()) -> m p
createPage _ = new

pageFromProtocol :: MonadIO m => (Page m -> m ()) -> [(Page m, [Protocol])] -> Maybe (Page m) -> Maybe URI -> m (Maybe (Page m))
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
            | protocol `elem` protos = do
                return $ Just page
            | otherwise = lookupProtocol plist

