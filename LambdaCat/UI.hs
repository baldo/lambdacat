{-# LANGUAGE MultiParamTypeClasses #-}

module LambdaCat.UI
    ( UI (..)
    )
where

import LambdaCat.Browser
import LambdaCat.Page

import Control.Monad.Trans

class (Browser browser m, Page page m, MonadIO m) => UI (ui browser page) browser page m where
    -- | Initializes the UI and returns an UI handle.
    init :: m ui

    -- | Creates the main UI widget for the browser (e.g. a window).
    newBrowser :: ui -> m browser

    -- | Embed the page into the given browser.
    embedPage :: ui -> browser -> page -> m ()

    -- | The main loop for the UI.
    mainLoop :: ui -> m ()

