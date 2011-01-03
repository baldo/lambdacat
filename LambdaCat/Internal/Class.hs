{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, FunctionalDependencies#-}

module LambdaCat.Internal.Class 
    ( UIClass (..)
    , ViewClass (..)
    , SupplierClass (..)
    , Callback

    , View (..) 
    , Supplier (..)
    )
where

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
    update ui meta f = f ui meta

    -- | Embed the view into the given browser.
    --   For this function the meta data COULD be undefined
    --   and is here to have a unique function interface
    --   for the supplier 
    embedView       :: View -> Callback ui meta 

    -- | Replace view with new view. The 'View' that
    -- should be replaced SHOULD be determind by @meta@.
    replaceView     :: View -> Callback ui meta
  
    -- | Inform @ui@ that @View@ has changed its URI.
    changedURI      :: View -> Callback ui meta

    -- | Inform @ui@ that @view@ has updated its title.
    changedTitle    :: View -> Callback ui meta 

    -- | Inform @ui@ that @view@ has changed its progress state. 
    changedProgress :: Int -> Callback ui meta 

    -- | Inform @ui@ that @view@ has changes its status.
    changedStatus   :: String -> Callback ui meta 

-- | Class of viewers, which can render and handle content behind a 'URI'.
class ViewClass view where
    -- | Creates a new view.
    new :: IO view

    -- | Ask the @view@ to embed its widget by calling the given function. 
    --   And give the callback function to the widget 
    embed :: UIClass ui meta
          => view -> (Widget -> IO ()) -> (Callback ui meta -> IO ()) -> IO ()

    -- | Destructor, allow cleaning up when @view@ is discarded. 
    destroy :: view -> IO ()

    -- | Ask view to load the given 'URI'
    load :: view -> URI -> IO Bool

    -- | Ask @view@ for the current uri, if not available 'nullURI'
    --   must be returned
    getCurrentURI :: view -> IO URI

    -- | Ask @view@ for the current title
    getCurrentTitle :: view -> IO String

    -- | Ask @view@ for the current progress, this must
    --   return a value between 0 and 100
    getCurrentProgress :: view -> IO Int

-- | Class of suppliers, which retrieve content and select appropiate viewers.
class SupplierClass supplier where
  -- | Ask @supplier@ for appropriated 'View' for 'URI'
  supplyView :: supplier -> URI -> IO (Maybe View) 
--  supplyContent :: TODO 

-- | Encapsulates any instance of 'ViewClass'
data View = forall view . (ViewClass view) => View view

instance ViewClass View where
    new                 = return (error "Can't create existential quantificated datatype")
    embed (View view) = embed view
    destroy (View view) = destroy view

    load (View view) = load view

    getCurrentURI (View view)   = getCurrentURI view
    getCurrentTitle (View view) = getCurrentTitle view
    getCurrentProgress (View view) = getCurrentProgress view

-- | Encapsulates any instance of 'SupplierClass'
data Supplier = forall supplier . (SupplierClass supplier) => Supplier supplier 

instance SupplierClass Supplier where
  supplyView (Supplier supplier) = supplyView supplier
