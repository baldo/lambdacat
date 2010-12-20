module LambdaCat.Session where

import LambdaCat.History (History)
import qualified LambdaCat.History as History 
import Data.Map (Map)
import qualified Data.Map as Map
import Network.URI (URI) 

data Session tabIdent tabmeta = Session 
  { sessionTabs      :: Map tabIdent (Tab tabIdent tabmeta) 
  , sessionTabActive :: Maybe tabIdent 
  }

data Tab tabIdent tabmeta  = Tab 
  { tabIdent   :: tabIdent 
  , tabMeta    :: tabmeta 
  , tabHistory :: History
  }

-- | create a new session with given session meta data.
newSession :: Session tabIdent tabMeta 
newSession = Session 
  { sessionTabs = Map.empty
  , sessionTabActive = Nothing
  }

-- | create a new tab and add uri to its initial history 
newTab :: Ord tabIdent 
       => tabIdent 
       -> tabMeta 
       -> URI 
       -> Session tabIdent tabMeta 
       -> Session tabIdent tabMeta
newTab ti tm uri session = 
  let tab   = Tab { tabIdent = ti , tabHistory = History.singleton uri , tabMeta = tm  }
      sessTabs = sessionTabs session
  in  session { sessionTabs = Map.insert ti tab sessTabs }


-- | Execute action on tab with ident in session
updateTab :: (Eq tabIdent,Ord tabIdent)
          => Session tabIdent tabMeta 
          -> tabIdent                   -- ^ ident of tab to be modified
          -> (tabMeta -> Maybe tabMeta) -- ^ if nothing is returned the tab gets deleted
          -> Session tabIdent tabMeta
updateTab session tabId f =
  let tabs    = sessionTabs session
      newTabs = Map.updateWithKey search tabId tabs -- TODO fix 
      search  = \ _ tab -> do meta <- f (tabMeta tab)
                              return $ tab { tabMeta = meta }
  in  session { sessionTabs = newTabs } 

getTab :: Ord tabIdent => Session tabIdent tabMeta -> tabIdent -> Maybe (Tab tabIdent tabMeta)
getTab session ti = 
  let sessTabs = sessionTabs session
  in  Map.lookup ti sessTabs 
