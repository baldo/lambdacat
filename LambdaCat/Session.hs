-- |
-- Module      : LambdaCat.Session
-- Copyright   : Andreas Baldeau, Daniel Ehlers
-- License     : BSD3
-- Maintainer  : Andreas Baldeau <andreas@baldeau.net>,
--               Daniel Ehlers <danielehlers@mindeye.net>
-- Stability   : Alpha
--
-- This module provides some basic session handling.

module LambdaCat.Session
    (
      -- * Datatypes
      MSession
    , Session (..)
    , Tab (..)

      -- * Concurrent session handling
    , newMSession
    , updateMSession
    , withMSession

      -- * Session access and manipulation
    , newSession
    , getSession

    , newTab
    , getTab
    , updateTab
    , deleteTab
    )
where

import Control.Concurrent.MVar
import Data.Map (Map)
import qualified Data.Map as Map
import Network.URI (URI)

import LambdaCat.History (History)
import qualified LambdaCat.History as History
import LambdaCat.View

-- | This datatype stores session related information.
data Session tabIdent tabmeta = Session
    { sessionTabs      :: Map tabIdent (Tab tabmeta)  -- ^ Map of tab's.
    , sessionTabActive :: Maybe tabIdent              -- ^ ID of the currently
                                                      -- active tab.
    }

-- | Datatypes for save concurrent access to session data.
newtype MSession tabIdent tabMeta = MSession
    { unMSession :: MVar (Session tabIdent tabMeta)  -- ^ Extract the stored
                                                     -- MVar.
    }

-- | Datatype representation of a tab.
data Tab tabmeta  = Tab
    { tabView    :: View     -- ^ Current view of the tab.
    , tabMeta    :: tabmeta  -- ^ Metadata connected to the tab.
    , tabHistory :: History  -- ^ History of the tab.
    }

-- | Create a (mutable) 'Session' for save concurrent access.
newMSession :: IO (MSession tabIdent tabMeta)
newMSession = return . MSession =<< newMVar newSession

-- | Create a session.
newSession :: Session tabIdent tabMeta
newSession = Session
    { sessionTabs      = Map.empty
    , sessionTabActive = Nothing
    }

-- | Create a new 'Tab' in the given session and add the given URI to its
-- initial 'History'.
newTab :: Ord tabIdent
       => tabIdent                  -- ^ The tab's identifier.
       -> View                      -- ^ The tab's view.
       -> tabMeta                   -- ^ Metadata to store with the tab.
       -> URI                       -- ^ The initial URI for the history.
       -> Session tabIdent tabMeta  -- ^ Session to add the tab to.
       -> Session tabIdent tabMeta  -- ^ Session containing the tab.
newTab ti view tm uri session =
    session { sessionTabs = Map.insert ti tab sessTabs }
  where
    tab = Tab
        { tabView    = view
        , tabHistory = History.singleton uri
        , tabMeta    = tm
        }

    sessTabs = sessionTabs session

-- | Update a tab within a 'Session'
updateTab :: (Eq tabIdent, Ord tabIdent)
          => Session tabIdent tabMeta              -- ^ The session to
                                                   -- operate on.
          -> tabIdent                              -- ^ Identifier of the tab
                                                   -- that should be modified.
          -> (Tab tabMeta -> Maybe (Tab tabMeta))  -- ^ Function that performs
                                                   -- the update. If 'Nothing'
                                                   -- is returned the tab gets
                                                   -- deleted.
          -> Session tabIdent tabMeta              -- ^ Modified session.
updateTab session tabId f =
    session { sessionTabs = newTabs }
  where
    tabs     = sessionTabs session
    newTabs  = Map.updateWithKey search tabId tabs  -- TODO fix
    search _ = f

-- | Get a tab specified by its identifier from the given session.
getTab
    :: Ord tabIdent
    => tabIdent                  -- ^ Identifier of the tab to get.
    -> Session tabIdent tabMeta  -- ^ Session to get the tab from.
    -> Maybe (Tab tabMeta)       -- ^ 'Nothing' if no tab for the identifier
                                 -- is found in the session.
getTab ti session =
    Map.lookup ti sessTabs
  where
    sessTabs = sessionTabs session

-- | Removes a tab from the session.
deleteTab
    :: Ord tabIdent
    => tabIdent                  -- ^ Identifier of the tab to remove.
    -> Session tabIdent tabMeta  -- ^ Session to remove from.
    -> Session tabIdent tabMeta  -- ^ Session without the tab.
deleteTab ident session =
    session { sessionTabs = Map.delete ident tabs }
  where
    tabs = sessionTabs session

-- | Return the current session stored in the 'MSession'.
getSession :: MSession tabIdent tabMeta -> IO (Session tabIdent tabMeta)
getSession = readMVar . unMSession

-- | Update the session stored in the given 'MSession' by applying the given
-- function and storing the result back in the MSession.
--
-- During this operation no other thread can access the session. If one tries
-- it will block.
updateMSession
    :: MSession tabIdent tabMeta
    -> (Session tabIdent tabMeta -> IO (Session tabIdent tabMeta, a))
    ->  IO a
updateMSession msession = modifyMVar (unMSession msession)

-- | Apply the given function to current session.
--
-- While the function is processed no other thread can access the session. If
-- one tries it will block.
withMSession
    :: MSession tabIdent tabMeta
    -> (Session tabIdent tabMeta -> IO a)
    -> IO a
withMSession msession = withMVar (unMSession msession)

