module LambdaCat.Supplier.Web
    ( webSupplier

    , module LambdaCat.Supplier
    ) where

import Data.List  (find)
import Data.Maybe (isJust)
import Network.URI 

import LambdaCat.Configure
import LambdaCat.Supplier
import LambdaCat.View

data WebSupplier = WebSupplier

webSupplier :: Supplier
webSupplier = Supplier WebSupplier

instance SupplierClass WebSupplier where
  supplyView _ uri =
    let viewers    = viewList lambdaCatConf 
        protocol   = uriScheme uri 
        mViewConst = find (\ (_vc, ps, _) -> isJust $ find (== protocol) ps) viewers
    in  case mViewConst of
      Just (vc,_,_) -> do 
        view <- createView vc

        _status <- load view uri
        return $ Just view
      Nothing -> return Nothing
