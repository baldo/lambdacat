module LambdaCat.Supplier
  ( SupplierClass (..)
  , Supplier (..)

  , Callback

  , supplyForView
  ) where

import Data.List  (find)
import Data.Maybe (isJust)
import Network.URI 

import LambdaCat.Class
import LambdaCat.Configure

-- | Selects a proper supplier for the 'URI'
supplyForView :: (Callback ui meta -> IO ()) -> (View -> Callback ui meta) -> URI -> IO () 
supplyForView callbackHdl embedHdl uri = 
  let suppliers = supplierList lambdaCatConf
      protocol = uriScheme uri 
      mSupply   = find (\ (_s, ps) -> isJust $ find (== protocol) ps) suppliers
  in  case mSupply of
    Just (supply,_) -> supplyView supply callbackHdl embedHdl uri
    Nothing         -> putStrLn $ "Can't find a supplier for protocol:" ++ protocol
