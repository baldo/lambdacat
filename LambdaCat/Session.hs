module LambdaCat.Session 
  ( MSession
  , Session (..)
  , Tab (..)
  
  , newMSession
  , updateMSession
  , withMSession

  , newSession
  , getSession
  , newTab
  , getTab 
  , updateTab
  , deleteTab
    )
  where

import LambdaCat.History (History)
import LambdaCat.View
import qualified LambdaCat.History as History 
import Data.Map (Map)
import qualified Data.Map as Map
import Network.URI (URI)
import Control.Concurrent.MVar

data Session tabIdent tabmeta = Session 
  { sessionTabs      :: Map tabIdent (Tab tabmeta) -- ^ 'Map' of 'Tab's
  , sessionTabActive :: Maybe tabIdent 
  }


-- | Concurrent save 'Session'
newtype MSession tabIdent tabMeta = MSession { unMSession :: MVar (Session tabIdent tabMeta )} 

data Tab tabmeta  = Tab 
  { tabView    :: View -- ^ Current 'View' of in a 'Tab'
  , tabMeta    :: tabmeta -- ^ Metadata connected to a 'Tab'
  , tabHistory :: History -- ^ 'History' of the 'Tab'
  }

-- | Create a concurrent save (mutable) 'Session'
newMSession :: IO (MSession tabIdent tabMeta)
newMSession = return . MSession =<< newMVar newSession

-- | Create a 'Session'
newSession :: Session tabIdent tabMeta 
newSession = Session 
  { sessionTabs = Map.empty
  , sessionTabActive = Nothing
  }

-- | Create a new 'Tab' and add uri to its initial 'History'
newTab :: Ord tabIdent 
       => tabIdent
       -> View 
       -> tabMeta 
       -> URI 
       -> Session tabIdent tabMeta 
       -> Session tabIdent tabMeta
newTab ti view tm uri session = 
  let tab   = Tab { tabView = view  , tabHistory = History.singleton uri , tabMeta = tm  }
      sessTabs = sessionTabs session
  in  session { sessionTabs = Map.insert ti tab sessTabs }


-- | Update tab within a 'Session'
updateTab :: (Eq tabIdent,Ord tabIdent)
          => Session tabIdent tabMeta -- ^ 'Session' to operate on
          -> tabIdent                   -- ^ ident of tab to be modified
          -> (Tab tabMeta -> Maybe (Tab tabMeta)) -- ^ if nothing is returned the tab gets deleted
          -> Session tabIdent tabMeta
updateTab session tabId f =
  let tabs    = sessionTabs session
      newTabs = Map.updateWithKey search tabId tabs -- TODO fix 
      search _ = f
  in  session { sessionTabs = newTabs } 

-- | Search 'Tab' in 'Session'
getTab :: Ord tabIdent => tabIdent -> Session tabIdent tabMeta -> Maybe (Tab tabMeta)
getTab ti session = 
  let sessTabs = sessionTabs session
  in  Map.lookup ti sessTabs 

-- | Remove 'Tab' with @tabIdent@ from 'Session'
deleteTab :: Ord tabIdent => tabIdent -> Session tabIdent tabMeta -> Session tabIdent tabMeta
deleteTab ident session = 
  let tabs = sessionTabs session
  in session { sessionTabs = Map.delete ident tabs } 

-- | Return current 'Session' from 'MSession'
getSession :: MSession tabIdent tabMeta -> IO (Session tabIdent tabMeta)
getSession = readMVar . unMSession

-- | Update 'MSession'
updateMSession :: MSession tabIdent tabMeta -> (Session tabIdent tabMeta -> IO (Session tabIdent tabMeta,a) ) ->  IO a
updateMSession msession = modifyMVar (unMSession msession)

-- | Execute function with current 'Session' state.
withMSession :: MSession tabIdent tabMeta -> (Session tabIdent tabMeta -> IO a) -> IO a 
withMSession msession = withMVar (unMSession msession)
