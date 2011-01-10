-- |
-- Module      : LambdaCat.Supplier.Web
-- Copyright   : Andreas Baldeau, Daniel Ehlers
-- License     : BSD3
-- Maintainer  : Andreas Baldeau <andreas@baldeau.net>,
--               Daniel Ehlers <danielehlers@mindeye.net>
-- Stability   : Alpha
--
-- This module provides the basic 'WebSupplier'. It doesn't really supply
-- content but creates a view that then loads the content.

module LambdaCat.Supplier.Web
    (
      -- * Supplier
      WebSupplier (..)

      -- * Module exports
    , module LambdaCat.Supplier
    )
where

import Data.List
    ( find
    )
import Data.Maybe
    ( isJust
    )
import Network.URI

import LambdaCat.Configure
import LambdaCat.Supplier
import LambdaCat.View

-- | The WebSupplier configuration datatype.
data WebSupplier = WebSupplierConf

instance SupplierClass WebSupplier where
    supplyView _ uri =
        let viewers    = viewList lambdaCatConf
            protocol   = uriScheme uri
            mViewConst =
               find (\(ViewSpec _ ps _) -> isJust $
                    find (== protocol) ps) viewers

        in  case mViewConst of
                Just (ViewSpec vc _ _) -> do
                    view <- createView vc

                    _status <- load view uri
                    return $ Just view

                Nothing ->
                    return Nothing

