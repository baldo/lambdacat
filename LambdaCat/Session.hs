module LambdaCat.Session where

import LambdaCat.History (History)
import qualified LambdaCat.History as History 
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
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
  { sessionTabs = IntMap.empty
  , sessionTabActive = Nothing
  }

-- | create a new tab and add uri to its initial history 
newTab :: Ord tabIdent 
       => tabIdent 
       -> tabMeta 
       -> URI 
       -> Session sessionMeta tabIdent tabMeta 
       -> (Int,Session sessionMeta tabIdent tabMeta)
newTab ti tm uri session = 
  let tab   = Tab { tabIdent = ti , tabHistory = History.singleton uri , tabMeta = tm  }
      sessTabs = sessionTabs session
      index    = newIndex sessTabs
  in  (index,session { sessionTabs = IntMap.insert index tab sessTabs })

newIndex :: IntMap a -> Int 
newIndex m | IntMap.null m  = 0
           | otherwise      = 1 + (fst $ IntMap.findMax m)

-- | Execute action on tab with ident in session
updateTab :: (Eq tabIdent,Ord tabIdent)
          => Session sessionMeta tabIdent tabMeta 
          -> tabIdent                   -- ^ ident of tab to be modified
          -> (tabMeta -> Maybe tabMeta) -- ^ if nothing is returned the tab gets deleted
          -> Session sessionMeta tabIdent tabMeta
updateTab session tabId f =
  let tabs    = sessionTabs session
      newTabs = IntMap.updateWithKey search tabId tabs -- TODO fix 
      search  = \ _ tab -> do meta <- f (tabMeta tab)
                              return $ tab { tabMeta = meta }
  in  session { sessionTabs = newTabs } 

getTab :: Session tabIdent tabMeta -> tabIdent -> Maybe (Tab tabIdent tabMeta)
-- TODO implement
