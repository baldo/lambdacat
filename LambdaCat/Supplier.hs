-- |
-- Module      : LambdaCat.Supplier
-- Copyright   : Andreas Baldeau, Daniel Ehlers
-- License     : BSD3
-- Maintainer  : Andreas Baldeau <andreas@baldeau.net>,
--               Daniel Ehlers <danielehlers@mindeye.net>
-- Stability   : Alpha
--
-- A supplier gets the content for a given URI and passes it to a matching
-- view.

module LambdaCat.Supplier
    (
      -- * Class and wrapper
      SupplierClass (..)
    , Supplier (..)

      -- * Callback type
    , Callback

      -- * Supplying for views
    , supplyForView
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
import LambdaCat.Internal.Class

-- | Selects a proper supplier for the given URI.
supplyForView
    :: (Callback ui meta -> IO ())
    -> (View -> Callback ui meta)
    -> URI
    -> IO ()
supplyForView callbackHdl embedHdl uri = do
    let suppliers = supplierList lambdaCatConf
        uri'      = modifySupplierURI lambdaCatConf uri
        protocol  = uriScheme uri'
        mSupply   = find (\(_s, ps) -> isJust $ find (== protocol) ps)
                         suppliers

    case mSupply of
        Just (supply, _) -> do
            mView <- supplyView supply uri'

            case mView of
                Just view ->
                    callbackHdl $ embedHdl view

                Nothing ->
                    putStrLn $ "Load view for unhandled uri" ++ show uri'

        Nothing ->
            putStrLn $ "Can't find a supplier for protocol:" ++ protocol

