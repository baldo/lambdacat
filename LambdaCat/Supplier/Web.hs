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

import Graphics.UI.Gtk.WebKit.Download
import Graphics.UI.Gtk.WebKit.NetworkRequest

-- | The WebSupplier configuration datatype.
data WebSupplier = WebSupplierConf
    { downloadDirectory :: FilePath
    }

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
    supplyDownload WebSupplierConf { downloadDirectory = dir } uri = do
        request <- networkRequestNew $ show uri
        download <- downloadNew request
        -- download uri to dir
        suggested <- downloadGetSuggestedFilename download
        case suggested of
            Just sug -> do
                downloadSetDestinationUri download $ "file://" ++ dir ++ "/" ++ sug
                downloadStart download
                return True
            Nothing -> do
                downloadSetDestinationUri download $ "file://" ++ dir ++ "/" ++ show uri
                downloadStart download
                return True
