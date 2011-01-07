-- |
-- Module      : LambdaCat.UI
-- Copyright   : Andreas Baldeau, Daniel Ehlers
-- License     : BSD3
-- Maintainer  : Andreas Baldeau <andreas@baldeau.net>,
--               Daniel Ehlers <danielehlers@mindeye.net>
-- Stability   : Alpha
--
-- This module exports the relevant typeclasses and datastructures to
-- implement your own UI for lambdacat.

module LambdaCat.UI
    ( module LambdaCat.Internal.Class
    , module LambdaCat.View
    )
where

import LambdaCat.Internal.Class
    ( UIClass (..)
    , Callback
    )
import LambdaCat.View
    ( ViewClass (..)
    , View (..)
    )

