module LambdaCat.History 
  ( History
 
  , singleton

  , back
  , forward
  , insert
  , insertAndForward
  , updateCurrent
  , current 

  , hasBack
  , hasForward
  , getForwards
  )
 where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Network.URI 
import Data.Maybe (isJust, fromJust)

-- | Directed tree with 'URI's as weights
type History = DTree URI

-- | Weighted directed tree
data DTree a = DTree 
  { dTreeWeight  :: a
  , dTreeBack    :: Maybe (Int,DTree a)
  , dTreeForward :: IntMap (DTree a)
  }
 deriving Show

-- | A tree with one node
singleton :: a -> DTree a
singleton weight = DTree 
  { dTreeWeight  = weight 
  , dTreeBack    = Nothing
  , dTreeForward = IntMap.empty
  }

-- | move back in a tree, return Nothing if can not move back
back :: DTree a -> Maybe (DTree a)
back dt = case dTreeBack dt of
  Just (index,dt') -> 
              let newDt   = dt { dTreeBack = Nothing }
                  forwMap = dTreeForward dt'
              in  Just $ dt' { dTreeForward = IntMap.insert index newDt forwMap }
  Nothing  -> Nothing

-- | indicated if a back operation on this tree is possible
hasBack :: DTree a -> Bool
hasBack = isJust . dTreeBack 

-- | move forward in a tree to select node
forward :: Int -> DTree a -> Maybe (DTree a)
forward index dt = 
  let forwMap = dTreeForward dt
      (mdt', forwMap') = IntMap.updateLookupWithKey (\ _ -> const Nothing) index forwMap
      newDt = dt { dTreeForward = forwMap' }
  in  case mdt' of
      Just dt' -> Just $ dt' { dTreeBack = Just (index,newDt) }
      Nothing  -> Nothing

-- | return the weight of the current node of a tree
current :: DTree a -> a 
current = dTreeWeight 

-- | replace weight at the current node
updateCurrent :: a -> DTree a -> DTree a
updateCurrent a tree = tree { dTreeWeight = a }

-- | indicates if a forward operation on this tree is possible
hasForward :: DTree a -> Bool
hasForward = not . IntMap.null . dTreeForward 

-- | get a list of possible forward nodes in a tree with weights on this nodes
getForwards :: DTree a -> [(Int,a)]
getForwards = map withSnd . IntMap.toList . dTreeForward
  where withSnd :: (Int,DTree a) -> (Int,a)
        withSnd (key,dt) = (key,dTreeWeight dt)

-- | insert a new node into the tree, adjacent to the current node
insert :: a -> DTree a -> DTree a 
insert weight dt = 
    let forwMap = dTreeForward dt
        newDt   = singleton weight
    in  dt { dTreeForward = IntMap.insert (newIndex forwMap) newDt forwMap }

-- | insert a new node into the tree, adjacent to the current node and then move forward to the new node
insertAndForward :: a -> DTree a -> DTree a
insertAndForward weight dt = 
    let forwMap = dTreeForward dt
        newDt   = singleton weight
        index   = newIndex forwMap
    in fromJust . forward index $ dt { dTreeForward = IntMap.insert index  newDt forwMap }

-- | internal function to generate next free index on a intmap
newIndex :: IntMap a -> Int 
newIndex m | IntMap.null m  = 0
           | otherwise      = 1 + fst (IntMap.findMax m)
