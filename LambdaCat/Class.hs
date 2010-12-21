{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification, MultiParamTypeClasses, FunctionalDependencies#-}

module LambdaCat.Class 
    ( UIClass (..)
    , ViewClass (..)
    , SupplierClass (..)
    , Callback

    , View (..) 
    , Supplier (..)
    )
where

import Data.Typeable
import Network.URI
import Graphics.UI.Gtk.Abstract.Widget

type Callback ui meta = ui -> meta -> IO ()


-- | Class of user interfaces for lambdacat 
class UIClass ui meta | ui -> meta where
    -- | Initializes the UI and returns an UI handle.
    init :: IO ui

    -- | The main loop for the UI.
    mainLoop :: ui -> IO ()

    -- | Function to apply to @View.new@ 
    update          :: ui -> meta -> Callback ui meta -> IO ()

    -- | Embed the view into the given browser.
    --   For this function the meta data COULD be undefined
    --   and is here to have a unique function interface
    --   for the supplier 
    embedView       :: View -> Callback ui meta 

    -- | Replace view with new view. The 'View' that
    -- should be replaced SHOULD be determind by @meta@.
    replaceView     :: View -> Callback ui meta
  
    -- | Inform @ui@ that @View@ has changed its URI
    changedURI      :: View -> Callback ui meta

    -- | Replace current title with the one from given view
    changedTitle    :: View -> Callback ui meta 

    changedProgress :: Int -> Callback ui meta 

    changedStatus   :: String -> Callback ui meta 

-- | Class of viewers, which can render and handle content behind a 'URI'.
class Typeable view => ViewClass view where
    -- | Creates a new view.
    new :: IO view

    -- | Ask the view to embed its widget by calling the given function. 
    --   And give the callback function to the widget 
    embed :: UIClass ui meta
          => view -> (Widget -> IO ()) -> (Callback ui meta -> IO ()) -> IO ()

    -- | Destructor, allow cleaning up when view is discarded. 
    destroy :: view -> IO ()

    -- | Ask view to load the given 'URI'
    load :: view -> URI -> IO Bool

    -- | generic informations on a view
    getCurrentURI :: view -> IO URI
    getCurrentTitle :: view -> IO String


-- | Class of suppliers, which retrieve content and select appropiate viewers.
class SupplierClass supplier where
  -- | Use of parameters
  --   view <- new 
  --   callbackfkt (actionfkt view)
  supplyView :: supplier -> (Callback ui meta -> IO ()) -> (View -> Callback ui meta) -> URI -> IO ()
--  supplyContent :: TODO 

-- | Encapsulates any instance of 'ViewClass'
data View = forall view . (Typeable view, ViewClass view) => View view

instance Typeable View where 
  typeOf (View view) = typeOf view

instance Show View where
    show (View view) = show $ typeOf view

instance ViewClass View where
    new                 = return (error "Can't create existential quantificated datatype")
    embed (View view) callback  = embed view callback
    destroy (View view) = destroy view

    load (View view) = load view

    getCurrentURI (View view)   = getCurrentURI view
    getCurrentTitle (View view) = getCurrentTitle view

data Supplier = forall supplier . (SupplierClass supplier) => Supplier supplier 

instance SupplierClass Supplier where
  supplyView (Supplier supplier) = supplyView supplier

eqType :: (Typeable a, Typeable b) => a -> b -> Bool
eqType a b = typeOf a == typeOf b

