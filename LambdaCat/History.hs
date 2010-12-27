module LambdaCat.History 
  ( History 
 
  , singleton

  , back
  , forward
  , insert
  , current 

  , hasBack
  , hasForward
  , getForwards
  )
 where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Network.URI 
import Data.Maybe (isJust)

type History = DTree URI

-- | Weighted directed Tree
data DTree a = DTree 
  { dTreeWeight  :: a
  , dTreeBack    :: Maybe (Int,DTree a)
  , dTreeForward :: IntMap (DTree a)
  }
 deriving Show

singleton :: a -> DTree a
singleton weight = DTree 
  { dTreeWeight  = weight 
  , dTreeBack    = Nothing
  , dTreeForward = IntMap.empty
  }

back :: DTree a -> Maybe (DTree a)
back dt = case dTreeBack dt of
  Just (index,dt') -> 
              let newDt   = dt { dTreeBack = Nothing }
                  forwMap = dTreeForward dt'
              in  Just $ dt' { dTreeForward = IntMap.insert index newDt forwMap }
  Nothing  -> Nothing

hasBack :: DTree a -> Bool
hasBack = isJust . dTreeBack 

forward :: Int -> DTree a -> Maybe (DTree a)
forward index dt = 
  let forwMap = dTreeForward dt
      (mdt', forwMap') = IntMap.updateLookupWithKey (\ _ -> const Nothing) index forwMap
      newDt = dt { dTreeForward = forwMap' }
  in  case mdt' of
      Just dt' -> Just $ dt' { dTreeBack = Just (index,newDt) }
      Nothing  -> Nothing

current :: DTree a -> a 
current = dTreeWeight 

hasForward :: DTree a -> Bool
hasForward = IntMap.null . dTreeForward 

getForwards :: DTree a -> [(Int,a)]
getForwards = map withSnd . IntMap.toList . dTreeForward
  where withSnd :: (Int,DTree a) -> (Int,a)
        withSnd (key,dt) = (key,dTreeWeight dt)

insert :: a -> DTree a -> DTree a 
insert weight dt = 
    let forwMap = dTreeForward dt
        newDt   = singleton weight
    in  dt { dTreeForward = IntMap.insert (newIndex forwMap) newDt forwMap }

newIndex :: IntMap a -> Int 
newIndex m | IntMap.null m  = 0
           | otherwise      = 1 + fst (IntMap.findMax m)
