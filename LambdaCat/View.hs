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

      -- * Wrapper types for the type classes
    , ViewEvent (..)
    )
where

import LambdaCat.Internal.Class

-- | Create a view.
--
-- Its type its specified by the configuration supplied.
createView :: ViewClass view => ViewConf view -> IO View
createView conf = do
    view <- new conf
    return $ View view
