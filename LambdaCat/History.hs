-- |
-- Module      : LambdaCat.History
-- Copyright   : Andreas Baldeau, Daniel Ehlers
-- License     : BSD3
-- Maintainer  : Andreas Baldeau <andreas@baldeau.net>,
--               Daniel Ehlers <danielehlers@mindeye.net>
-- Stability   : Alpha
--
-- This module provides lambdacat's history functionality. The history is
-- not stored linear as in other browsers, it is stored as a tree. This way
-- navigating backwards and then along another path no navigation history is
-- lost.

module LambdaCat.History
    (
      -- * The data structure
      History

      -- * Construction
    , singleton

      -- * Navigation
    , back
    , forward

      -- * Modification
    , insert
    , insertAndForward
    , updateCurrent

      -- * Query
    , current
    , hasBack
    , hasForward
    , getForwards
    )
where

import Data.IntMap
    ( IntMap
    )
import qualified Data.IntMap as IntMap
import Data.Maybe
    ( fromJust
    , isJust
    )
import Network.URI

-- | Directed tree with 'URI's as weights.
type History = DTree URI

-- | Weighted directed tree
data DTree a = DTree
    { dTreeWeight  :: a                     -- ^ Weight of the node. 
    , dTreeBack    :: Maybe (Int, DTree a)  -- ^ The nodes parent, if any.
                                            -- The Int is the number by which
                                            -- this node can be reached from
                                            -- its parent.
    , dTreeForward :: IntMap (DTree a)      -- ^ The numbered childs.
    }
  deriving Show

-- | A tree with one node.
singleton
    :: a        -- ^ The weight for the node.
    -> DTree a  -- ^ The new tree.
singleton weight = DTree
    { dTreeWeight  = weight
    , dTreeBack    = Nothing
    , dTreeForward = IntMap.empty
    }

-- | Move backwards in the tree.
back
    :: DTree a          -- ^ Tree to navigate in.
    -> Maybe (DTree a)  -- ^ Just the parent node or Nothing if none.
back dt =
    case dTreeBack dt of
        Just (index, dt') ->
            let newDt   = dt { dTreeBack = Nothing }
                forwMap = dTreeForward dt'
            in  Just dt' { dTreeForward = IntMap.insert index newDt forwMap }

        Nothing ->
            Nothing

-- | Indicates, if a back operation on the tree is possible.
hasBack :: DTree a -> Bool
hasBack = isJust . dTreeBack

-- | Move forward in the tree.
forward
    :: Int              -- ^ Number of the child node to navigate to.
    -> DTree a          -- ^ The tree to navigate in.
    -> Maybe (DTree a)  -- ^ Just the child node or Nothing if not existing.
forward index dt =
    case mdt' of
        Just dt' ->
            Just $ dt' { dTreeBack = Just (index, newDt) }

        Nothing ->
            Nothing

  where
    forwMap = dTreeForward dt

    (mdt', forwMap') =
        IntMap.updateLookupWithKey (\_ -> const Nothing) index forwMap

    newDt = dt { dTreeForward = forwMap' }

-- | Returns the weight of the trees current node.
current :: DTree a -> a
current = dTreeWeight

-- | Replace the weight at the current node.
updateCurrent :: a -> DTree a -> DTree a
updateCurrent a tree = tree { dTreeWeight = a }

-- | Indicates if a forward operation on this tree is possible.
hasForward :: DTree a -> Bool
hasForward = not . IntMap.null . dTreeForward

-- | Returns a list of child nodes identified by its numbers and their
-- weights.
getForwards :: DTree a -> [(Int, a)]
getForwards =
    map withSnd . IntMap.toList . dTreeForward

  where
    withSnd :: (Int, DTree a) -> (Int, a)
    withSnd (key, dt) = (key, dTreeWeight dt)

-- | Insert a new child node adjacent to the trees current node.
insert :: a -> DTree a -> DTree a
insert weight dt =
    dt { dTreeForward = IntMap.insert (newIndex forwMap) newDt forwMap }

  where
    forwMap = dTreeForward dt
    newDt   = singleton weight

-- | Insert a new child node adjacent to the tress current node and then
-- move forward to it.
insertAndForward :: a -> DTree a -> DTree a
insertAndForward weight dt =
    fromJust . forward index $
        dt { dTreeForward = IntMap.insert index newDt forwMap }

  where
    forwMap = dTreeForward dt
    newDt   = singleton weight
    index   = newIndex forwMap

-- | Internal function to generate the next free index on an IntMap.
newIndex :: IntMap a -> Int
newIndex m
    | IntMap.null m = 0
    | otherwise     = 1 + fst (IntMap.findMax m)

