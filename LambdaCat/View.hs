module LambdaCat.View
    ( ViewClass (..)
    , View (..)

    , createView

    , Callback
    )
where

import LambdaCat.Internal.Class

-- | Create a 'View' from a configuration constant.
createView :: View -> IO View
createView (View v) = return . View =<< createView_ v

createView_ :: (ViewClass view) 
           => view
           -> IO view
createView_ _ = new
