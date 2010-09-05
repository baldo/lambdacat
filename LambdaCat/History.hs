module LambdaCat.History
    ( History
    , HistoryMap

    -- history functions
    , newHistory
    , historyAdd
    , historyForward
    , historyBackward

    -- map functions
    , newHistoryMap
    , 
    ) where

import LambdaCat.Page

import Control.Concurrent.MVar
import Data.Map (Map)
import qualified Data.Map as Map
import Network.URI

newtype  History = History { unHistory :: ([HistoryEntry], HistoryEntry, [HistoryEntry]) }
    deriving Show

type HistoryEntry = URI

newHistory :: HistoryEntry -> History
newHistory uri = History ([], uri, [])

historyAdd :: History -> HistoryEntry -> History
historyAdd (History (past, present, _)) uri = History (present : past, uri, [])

historyBackward :: History -> (HistoryEntry, History)
historyBackward h@(History ([]  , pres, _))   = History (pres, h)
historyBackward (History (p : ps, pres, fut)) = History (p, (ps, p, pres : fut))

historyForward :: History -> (HistoryEntry, History)
historyForward h@(History (_, pres, []))      = History (pres, h)
historyForward (History (past, pres, f : fs)) = History (f, (pres : past, f, fs))

-- Data structure for handling multiple Historys 

newtype HistoryManager key = HM { unMap :: MVar (HistoryMap key) }

type HistoryMap key = Map key HistoryEntry

newHistoryMap :: Ord key => IO (HistoryMap key)
newHistoryMap = do
    m <- newMVar Map.empty
    return (HM m)

createHistoryForKey :: Ord key => key -> HistoryEntry -> HistoryManager -> IO ()
createHistoryForKey key entry (HM mvar) =
   modifyMVar_ mvar (return . Map.insert key (newHistory entry))


