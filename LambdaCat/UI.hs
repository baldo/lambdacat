{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, ExistentialQuantification, FlexibleInstances #-}

module LambdaCat.UI
    ( --UIClass (..)
    )
where
{-
import LambdaCat.Browser
import LambdaCat.Page

import Control.Monad.Trans

class MonadIO m => UIClass ui m where
    -- | Initializes the UI and returns an UI handle.
    init :: m ui

    -- | Creates the main UI widget for the browser (e.g. a window).
    newBrowser :: ui -> m BrowserID

    -- | Embed the page into the given browser.
    embedPage :: ui -> BrowserID -> Page m -> m ()

    -- | Checks if a page is child of this brower/ui
    containsPage :: ui -> Page m -> m Bool
    uriChanged   :: ui -> Page m -> m ()

    update :: ui -> Page m -> m ()

    -- | The main loop for the UI.
    mainLoop :: ui -> m ()
-}
