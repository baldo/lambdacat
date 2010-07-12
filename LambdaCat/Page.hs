{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, ExistentialQuantification, FlexibleInstances #-}

module LambdaCat.Page
    ( Page (..)
    , PageClass (..)
    , HasWidget (..)
    , pageFromURI 
    )
where

import LambdaCat.Protocol

import Control.Monad.Trans
import Network.URI
import Graphics.UI.Gtk.Abstract.Widget

class MonadIO m => PageClass page m where
    -- | Creates a new page.
    new :: m page

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

data Page m = forall hw w . (HasWidget hw w, PageClass hw m) => Page hw

instance MonadIO m => PageClass (Page m) m where
   new = return (error "Can't create existential quantificated datatype")  

   load (Page p) = load p
   
   back (Page p) = back p
   forward (Page p) = forward p
   stop (Page p) = stop p
   reload (Page p) = reload p
   
   getBackHistory (Page p) = getBackHistory p
   getForwardHistory (Page p) = getForwardHistory p

pageFromURI :: (PageClass page m,MonadIO m) => [((m page),[Protocol])] -> (Maybe URI) -> m (Maybe page)
pageFromURI _ Nothing = return Nothing
pageFromURI pList (Just uri) = do
    let schema = uriScheme uri
        pages  = map (\ (f,_) -> f) $ filter (\ (_,protos) -> hasProtocol schema protos) pList 
    if null pages 
     then return Nothing
     else do page <- (head pages)
             load page uri
             return (Just page)
 where hasProtocol prot = not . null . filter (== prot) 
