module LambdaCat.Supplier
  ( SupplierClass (..)
  , Supplier (..)

  , Callback

  , supplyForView
  ) where

import Data.List  (find)
import Data.Maybe (isJust)
import Network.URI 

import LambdaCat.Configure
import LambdaCat.Internal.Class

-- | Selects a proper supplier for the 'URI'
supplyForView :: (Callback ui meta -> IO ()) -> (View -> Callback ui meta) -> URI -> IO () 
supplyForView callbackHdl embedHdl uri = 
  let suppliers = supplierList lambdaCatConf
      protocol = uriScheme uri 
      mSupply   = find (\ (_s, ps) -> isJust $ find (== protocol) ps) suppliers
  in  case mSupply of
    Just (supply,_) -> do 
      mView <- supplyView supply uri
      case mView of
        Just view -> callbackHdl (embedHdl view)
        Nothing   -> putStrLn $ "Load view for unhandled uri" ++ show uri
    Nothing         -> putStrLn $ "Can't find a supplier for protocol:" ++ protocol
