{-# LANGUAGE FlexibleInstances,MultiParamTypeClasses #-}

module LambdaCat.Page.Poppler 
    ( PopplerPage
    ) where

import LambdaCat.Page

import Control.Concurrent
import Control.Monad.Trans
import System.Glib.GObject
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Poppler.Document hiding (PageClass)
import Graphics.UI.Gtk.Poppler.Page
import Graphics.UI.Gtk.Abstract.Object
import Network.URI
import Data.Maybe
import Control.Monad
import Graphics.Rendering.Cairo


data PopplerPage = PopplerPage
    { pageArea     :: DrawingArea
    , pageScrollable :: ScrolledWindow
    , pageDocument :: MVar (Maybe Document)
    , pageNumber   :: MVar Int 
    }

-- The following is probably a bad idea, maybe it throws an LALALALA error.
-- There is no documented way to instantiate a widget....

instance ObjectClass PopplerPage 

instance GObjectClass PopplerPage where
    toGObject PopplerPage { pageScrollable = scroll } = toGObject scroll 
    unsafeCastGObject o = (error "LALALALALA")
   
instance WidgetClass PopplerPage 

-- This is a better idea ;)

instance MonadIO m => PageClass PopplerPage m where 
    new = liftIO $ do
        area <- drawingAreaNew
        scrollWindow <- scrolledWindowNew Nothing Nothing
        doc <- newMVar Nothing
        num <- newMVar 0

        scrolledWindowAddWithViewport scrollWindow area
        scrolledWindowSetPolicy scrollWindow PolicyAutomatic PolicyAutomatic

        let popplerPage = PopplerPage { pageArea = area
                                      , pageScrollable = scrollWindow
                                      , pageDocument = doc
                                      , pageNumber = num 
                                      }
        
        area `on` exposeEvent $ tryEvent $ viewerDraw popplerPage

        return popplerPage

    load page@(PopplerPage { pageArea = area,  pageDocument = mdoc }) uri = liftIO $ do
        doc <- liftM (fromMaybe (error "Error opening pdf file.")) (documentNewFromFile (uriString) Nothing)
        _ <-  takeMVar mdoc
        putMVar mdoc (Just doc)
     where uriString = uriToString id uri ""

viewerDraw :: PopplerPage -> EventM EExpose ()
viewerDraw viewer = do
  let area = pageArea viewer
  pN       <- liftIO $ readMVar $ pageNumber viewer
  mayBeDoc <- liftIO $ readMVar $ pageDocument viewer-- TODO replace with takeMVar
  (winWidth, winHeight) <- eventWindowSize                    
  case mayBeDoc of
    Nothing -> return ()
    (Just doc) ->  liftIO $ do
        page       <- documentGetPage doc pN
        frameWin   <- widgetGetDrawWindow area
        (docWidth, docHeight) <- pageGetSize page
        widgetSetSizeRequest area (truncate docWidth) (truncate docHeight)

        renderWithDrawable frameWin $ do 
            setSourceRGB 1.0 1.0 1.0
            rectangle 0.0 0.0 winWidth winHeight
            fill
            let scaleX = winWidth / docWidth 
            scale scaleX scaleX
            pageRender page

eventWindowSize :: EventM EExpose (Double, Double)      
eventWindowSize = do
    dr    <- eventWindow
    (w,h) <- liftIO $ drawableGetSize dr
    return $ if w * h > 1
               then (fromIntegral w, fromIntegral h)
               else (1,1)
