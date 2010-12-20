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
supplyForView callbackHdl embedHdl uri = do
  let suppliers = supplierList lambdaCatConf
      protocol = uriScheme uri 
      mSupply   = find (\ (s,ps) -> isJust $ find (==protocol) ps) suppliers
  case mSupply of
    Just (supply,_) -> supplyView supply callbackHdl embedHdl uri
    Nothing         -> return () -- maybe an error or something


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
        return ()
      Nothing -> return ()
        
    
