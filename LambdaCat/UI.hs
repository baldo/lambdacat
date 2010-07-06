{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, ExistentialQuantification #-}

module LambdaCat.UI
    ( UI (..)
    , UIClass (..)
    )
where

import LambdaCat.Browser
import LambdaCat.Page

import Control.Monad.Trans

data UI m = forall u b p . UIClass u b p m => UI u

class (BrowserClass browser m, PageClass page m, MonadIO m) => UIClass ui browser page m | ui -> browser page where
    -- | Initializes the UI and returns an UI handle.
    init :: m ui

    -- | Creates the main UI widget for the browser (e.g. a window).
    newBrowser :: ui -> m browser

    -- | Embed the page into the given browser.
    embedPage :: ui -> browser -> page -> m ()

    -- | The main loop for the UI.
    mainLoop :: ui -> m ()

