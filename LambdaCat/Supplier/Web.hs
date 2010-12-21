module LambdaCat.Supplier.Web
    ( webSupplier
    ) where

import Data.List  (find)
import Data.Maybe (isJust)
import Network.URI 

import LambdaCat.Class
import LambdaCat.Configure

data WebSupplier = WebSupplier

webSupplier :: Supplier
webSupplier = Supplier WebSupplier

instance SupplierClass WebSupplier where
  supplyView _ callbackHdl embedHdl uri =
    let viewers    = viewList lambdaCatConf 
        protocol   = uriScheme uri 
        mViewConst = find (\ (_vc, ps, _) -> isJust $ find (== protocol) ps) viewers
    in  case mViewConst of
      Just (vc,_,_) -> do 
        view <- createView vc
        callbackHdl (embedHdl view)
        _status <- load view uri
        return ()
      Nothing -> putStrLn $ "Can't find a view for protocol:" ++ protocol 
