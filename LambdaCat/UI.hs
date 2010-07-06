{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, ExistentialQuantification, FlexibleInstances #-}

module LambdaCat.UI
    ( UI (..)
    , UIClass (..)
    )
where

import LambdaCat.Browser
import LambdaCat.Page

import Control.Monad.Trans

class (BrowserClass browser m, PageClass page m, MonadIO m) => UIClass ui browser page m | ui -> browser page where
    -- | Initializes the UI and returns an UI handle.
    init :: m ui

    -- | Creates the main UI widget for the browser (e.g. a window).
    newBrowser :: ui -> m browser

    -- | Embed the page into the given browser.
    embedPage :: ui -> browser -> page -> m ()

    -- | The main loop for the UI.
    mainLoop :: ui -> m ()

data UI b p m = forall u . (UIClass u b p m) => UI u

instance (BrowserClass browser m, PageClass page m, MonadIO m) => UIClass (UI browser page m) browser page m where
    init = return (error "Can't initialize existential quantificated datatype")

    newBrowser (UI u) = do
        b <- newBrowser u
        return b 

    embedPage (UI u) b p = do
        embedPage u b p 
        return ()

    mainLoop (UI u) = mainLoop u 
