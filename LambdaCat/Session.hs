module LambdaCat.Session where

import LambdaCat.History (History)
import qualified LambdaCat.History as History 
import Data.Map (Map)
import qualified Data.Map as Map
import Network.URI (URI)
import LambdaCat.Class
import Control.Concurrent.MVar

data Session tabIdent tabmeta = Session 
  { sessionTabs      :: Map tabIdent (Tab tabmeta) 
  , sessionTabActive :: Maybe tabIdent 
  }

newtype MSession tabIdent tabMeta = MSession { unMSession :: MVar (Session tabIdent tabMeta )} 

-- Do we really need the tabmeta in here
-- it should be passed throught the ui and callback Hdl on the View side 
data Tab tabmeta  = Tab 
  { tabView    :: View
  , tabMeta    :: tabmeta 
  , tabHistory :: History
  }

newMSession :: IO (MSession tabIdent tabMeta)
newMSession = return . MSession =<< newMVar newSession

-- | create a new session with given session meta data.
newSession :: Session tabIdent tabMeta 
newSession = Session 
  { sessionTabs = Map.empty
  , sessionTabActive = Nothing
  }

-- | create a new tab and add uri to its initial history 
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


-- | Execute action on tab with ident in session
updateTab :: (Eq tabIdent,Ord tabIdent)
          => Session tabIdent tabMeta 
          -> tabIdent                   -- ^ ident of tab to be modified
          -> (Tab tabMeta -> Maybe (Tab tabMeta)) -- ^ if nothing is returned the tab gets deleted
          -> Session tabIdent tabMeta
updateTab session tabId f =
  let tabs    = sessionTabs session
      newTabs = Map.updateWithKey search tabId tabs -- TODO fix 
      search  = \ _ tab -> do newtab <- f tab
                              return $ newtab
  in  session { sessionTabs = newTabs } 

getTab :: Ord tabIdent => Session tabIdent tabMeta -> tabIdent -> Maybe (Tab tabMeta)
getTab session ti = 
  let sessTabs = sessionTabs session
  in  Map.lookup ti sessTabs 

getSession :: MSession tabIdent tabMeta -> IO (Session tabIdent tabMeta)
getSession = readMVar . unMSession

withMSession :: (Session tabIdent tabMeta -> Session tabIdent tabMeta) -> MSession tabIdent tabMeta -> IO () 
withMSession f (MSession mvar) = withMVar mvar (return . f) >> return ()
