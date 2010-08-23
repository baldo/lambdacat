module LambdaCat.History 
    ( History

    , newHistory
    , historyAdd
    , historyForward
    , historyBackward
    ) where

import LambdaCat.Page

import Network.URI

newtype  History = History { unHistory :: ([HistoryEntry],HistoryEntry,[HistoryEntry]) }
    deriving Show

type HistoryEntry = URI

newHistory :: URI -> History
newHistory uri = History ([],uri,[])

historyAdd :: History -> URI -> History
historyAdd (History (past,present,_)) uri = History (present:past,uri,[])

historyBackward :: History -> (URI,History)
historyBackward h@(History ([]  ,pres,_)) = History (pres,h)
historyBackward (History (p:ps,pres,fut)) = History (p,(ps,p,pres:fut))

historyForward :: History -> (URI,History)
historyForward h@(History (_,pres,[]))    = History (pres,h)
historyForward (History (past,pres,f:fs)) = History (f,(pres:past,f,fs))
