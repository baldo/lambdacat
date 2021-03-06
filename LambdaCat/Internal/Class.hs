{-# LANGUAGE ExistentialQuantification
           , FunctionalDependencies
           , MultiParamTypeClasses
           , TypeFamilies
  #-}

-- |
-- Module      : LambdaCat.Internal.Class
-- Copyright   : Andreas Baldeau, Daniel Ehlers
-- License     : BSD3
-- Maintainer  : Andreas Baldeau <andreas@baldeau.net>,
--               Daniel Ehlers <danielehlers@mindeye.net>
-- Stability   : Alpha
--
-- This module provides the basic type classes and data types that cannot be
-- put into other modules due to cyclic dependencies.
--
-- All the classes are reexported by other modules, so there is no need to
-- expose this module.

module LambdaCat.Internal.Class
    (
      -- * Type classes
      UIClass (..)
    , ViewClass (..)
    , SupplierClass (..)

      -- * The @Callback@ type
    , Callback

      -- * Event reporting
    , ViewEvent (..)

      -- * Wrapper types for the type classes
    , View (..)
    )
where

import Network.URI

import Graphics.UI.Gtk.Abstract.Widget

-- | Datatype for callback functions.
type Callback ui meta = ui -> meta -> IO ()

-- | Datatype for reporting view events to the ui
data ViewEvent = URIChanged
               | TitleChanged
               | ProgressChanged Int
               | StatusChanged String

-- | Class of user interfaces for lambdacat.
class UIClass ui meta | ui -> meta where
    -- | Configuration datatype.
    data UIConf ui meta :: *

    -- | Initializes the UI and returns an UI handle.
    init :: UIConf ui meta -> IO ui

    -- | The main loop for the UI.
    mainLoop :: ui -> IO ()

    -- | Function to give to 'embed'.
    update :: ui -> meta -> Callback ui meta -> IO ()
    update ui meta f = f ui meta

    -- | Embed the view into the given UI.
    --
    -- For this function the meta data COULD be undefined but is here to have
    -- a unique function interface for the supplier.
    embedView       :: View -> Callback ui meta

    -- | Replace one view with a new view. The view that should be replaced
    -- SHOULD be determind by @meta@. The type of the new view is determined
    -- by the first argument.
    replaceView     :: View -> Callback ui meta

    -- | Inform the @ui@ that an event has happen in a 'View'
    updateView      :: View -> ViewEvent -> Callback ui meta

-- | Class of viewers, that can render and handle content behind an 'URI'.
class ViewClass view where
    -- | Configuration datatype.
    data ViewConf view :: *

    -- | Creates a new view.
    new :: ViewConf view -> IO view

    -- | Ask the view to embed its widget by calling the given function.
    -- Also give the callback function to the widget.
    embed :: UIClass ui meta
          => view                         -- ^ The view to embed.
          -> (Widget -> IO ())            -- ^ Function to embed the widget.
          -> (Callback ui meta -> IO ())
          -> IO ()

    -- | Destructor, allow cleaning up when the view is discarded.
    destroy :: view -> IO ()

    -- | Ask the view to load the given URI.
    load :: view -> URI -> IO Bool

    -- | Ask the view for the current URI. If no URI is available, 'nullURI'
    -- must be returned.
    getCurrentURI :: view -> IO URI

    -- | Ask the view for the current title.
    getCurrentTitle :: view -> IO String

    -- | Ask the view for the current progress. This must return a value
    -- between 0 and 100
    getCurrentProgress :: view -> IO Int

-- | Class of suppliers, which retrieve content and select appropiate viewers.
class SupplierClass conf where
    -- | Ask the supplier for an appropriated view for the URI.
    -- This function is optional, the default always return Nothing.
    supplyView :: conf -> URI -> IO (Maybe View)
    supplyView _ _ = return Nothing

    -- supplyContent :: TODO

    -- | Ask the supplier to handle a download of the given uri. If the
    -- supplier decides to handle this download he must return True, otherwise
    -- False. This function is optional, the default implementation
    -- always returns False.
    -- TODO Add callback function as parameter and define a transfer manager
    -- class.
    supplyDownload :: conf -> URI -> IO Bool
    supplyDownload _ _ = return False

-- | Encapsulates any instance of ViewClass.
data View = forall view . ViewClass view => View view

instance ViewClass View where
    data ViewConf View
    new _ = return (error "Can't create existential quantificated datatype")

    embed              (View view) = embed view
    destroy            (View view) = destroy view

    load               (View view) = load view

    getCurrentURI      (View view) = getCurrentURI view
    getCurrentTitle    (View view) = getCurrentTitle view
    getCurrentProgress (View view) = getCurrentProgress view
