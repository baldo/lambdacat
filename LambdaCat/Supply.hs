module LambdaCat.Supply 
  ( supplyForView
  ) where

import Data.List (find)
import Network.URI 

import LambdaCat.Class
import LambdaCat.Configure


-- | Selects a proper supplier for the 'URI'
supplyForView :: (Callback ui meta -> IO ()) -> (View -> Callback ui meta) -> URI -> IO () 
supplyForView callbackHdl embedHdl uri = do
  let suppliers = supplierList lambdaCatConf
      protocoll = uriScheme uri 
      mSupply   = find (\ (s,ps) -> null $ filter (==protocoll) ps) suppliers
  case mSupply of
    Just (supply,_) -> supplyView supply callbackHdl embedHdl uri
    Nothing         -> return () -- maybe an error or something
