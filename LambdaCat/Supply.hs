module LambdaCat.Supply 
  ( supplyForView
  
  , webSupplier
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
      mSupply   = find (\ (s,ps) -> isJust $ find (==protocol) ps) suppliers
  in  case mSupply of
    Just (supply,_) -> supplyView supply callbackHdl embedHdl uri
    Nothing         -> putStrLn $ "Can't find a supplier for protocol:" ++ protocol


data WebSupplier = WebSupplier

webSupplier :: Supplier
webSupplier = Supplier WebSupplier

instance SupplierClass WebSupplier where
  supplyView _ callbackHdl embedHdl uri =
    let viewers    = viewList lambdaCatConf 
        protocol   = uriScheme uri 
        mViewConst = find (\ (vc,ps,_) -> isJust $ find (== protocol) ps) viewers
    in  case mViewConst of
      Just (vc,_,_) -> do 
        view <- createView vc
        callbackHdl (embedHdl view)
        _status <- load view uri
        return ()
      Nothing -> putStrLn $ "Can't find a view for protocol:" ++ protocol 
