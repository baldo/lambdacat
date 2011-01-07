-- |
-- Module      : LambdaCat.View
-- Copyright   : Andreas Baldeau, Daniel Ehlers
-- License     : BSD3
-- Maintainer  : Andreas Baldeau <andreas@baldeau.net>,
--               Daniel Ehlers <danielehlers@mindeye.net>
-- Stability   : Alpha
--
-- This module provides view related classes, types and functions.

module LambdaCat.View
    (
      -- * Class and wrapper
      ViewClass (..)
    , View (..)

      -- * Construction
    , createView

      -- * Callback type
    , Callback
    )
where

import LambdaCat.Internal.Class

-- | Create a view.
--
-- Its type is specified by the first parameter. This should be a view of the
-- same type or one of the constants exported in the corresponding
-- @LambdaCat.View.*@ modules.
createView :: View -> IO View
createView (View v) = return . View =<< createView_ v

-- | Helper function that assures the view of the correct type is created.
createView_ :: (ViewClass view) => view -> IO view
createView_ _ = new

