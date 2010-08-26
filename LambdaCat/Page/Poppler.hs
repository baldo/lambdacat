{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}

module LambdaCat.Page.Poppler
    ( PopplerPage

    , popplerPage
    ) where

import qualified LambdaCat.Page as Page
import LambdaCat.Page hiding (Page)
import LambdaCat.Page.Poppler.PageLayout
import LambdaCat.Utils

import Control.Concurrent
import Control.Monad
import Data.Typeable
import Graphics.UI.Gtk hiding (Point)
import Graphics.UI.Gtk.Poppler.Document hiding (PageClass, PageLayout)
import Graphics.UI.Gtk.Poppler.Page
import Network.URI
import Graphics.Rendering.Cairo


data PopplerPage = PopplerPage
    { pageArea       :: DrawingArea
    , pageScrollable :: ScrolledWindow
    , pageDocument   :: MVar (Maybe Document)
    , pageGeometry   :: MVar (Maybe DocumentGeometry)
    , pageNumber     :: MVar Int
    , pageURI        :: MVar URI
    }
  deriving (Eq, Typeable)

popplerPage :: Page.Page
popplerPage = Page.Page (undefined :: PopplerPage)

type Point = (Double, Double)

instance HasWidget PopplerPage ScrolledWindow where
    getWidget = pageScrollable

instance PageClass PopplerPage where
    new _ = do
        area <- drawingAreaNew
        scrollWindow <- scrolledWindowNew Nothing Nothing
        doc <- newMVar Nothing
        geo <- newMVar Nothing
        num <- newMVar 0
        uri <- newMVar nullURI

        scrolledWindowAddWithViewport scrollWindow area
        scrolledWindowSetPolicy scrollWindow PolicyAutomatic PolicyAutomatic

        let pPage = PopplerPage { pageArea = area
                                , pageScrollable = scrollWindow
                                , pageDocument = doc
                                , pageGeometry = geo
                                , pageNumber = num
                                , pageURI = uri
                                }

        _ <- area `on` exposeEvent $ tryEvent $ viewerDraw pPage

        return pPage

    destroy _ = return ()

    load PopplerPage { pageArea = area,  pageDocument = mdoc, pageGeometry = mGeo,  pageURI = muri } uri = do
        mDoc <- documentNewFromFile uriString Nothing
        case mDoc of
            Nothing -> do
                $pinfo putStrLn "Error opening pdf file"
                return False
            Just doc -> do
                _ <-  takeMVar mdoc
                putMVar mdoc (Just doc)
                _ <- takeMVar muri
                putMVar muri uri
                _ <- takeMVar mGeo
                geo <- (toDocumentGeometry doc)
                putMVar mGeo (Just geo)
                widgetQueueDraw area
                return True
     where uriString = uriToString id uri ""

    getCurrentURI page = do
        let mURI = (pageURI page)
        withMVar mURI return

    getCurrentTitle page = do
        let mDoc = (pageDocument page)
        withMVar mDoc $ \ m -> case m of
                (Just d) -> get d documentTitle
                _ -> return ""

viewerDraw :: PopplerPage -> EventM EExpose ()
viewerDraw viewer = do
  let area = pageArea viewer
      scroll = pageScrollable viewer
  pN <- liftIO $ readMVar $ pageNumber viewer
  mayBeDoc <- liftIO $ takeMVar $ pageDocument viewer -- TODO replace with takeMVar
  (winWidth, winHeight) <- liftIO $ widgetGetSize scroll
  hCurrent <- liftIO $ adjustmentGetValue =<< scrolledWindowGetHAdjustment scroll
  vCurrent <- liftIO $ adjustmentGetValue =<< scrolledWindowGetVAdjustment scroll
  case mayBeDoc of
    Nothing -> return ()
    (Just doc) -> do
        frameWin  <- liftIO $ widgetGetDrawWindow area
        Just geo  <- liftIO $ readMVar $ pageGeometry viewer

        let posLayout = continue (fitPage 2) pN (fromIntegral winWidth, fromIntegral winHeight) geo
            (widgetWidth, widgetHeight) = pageLayoutSize posLayout
        posPages <- liftIO $ fromPageLayout posLayout doc
        liftIO $ widgetSetSizeRequest area (truncate widgetWidth) (truncate widgetHeight)

        mapM_ (\ (page, scal, p@(x0, y0), s@(x1, y1)) -> liftIO $ renderWithDrawable frameWin $
           when (shouldDraw (p, s) ((hCurrent, vCurrent), (fromIntegral winWidth, fromIntegral winHeight))) $ do
                setSourceRGB 1.0 1.0 1.0
                rectangle x0 y0 x1 y1
                fill
                translate x0 y0
                scale scal scal
                pageRender page
            ) posPages
  liftIO $ putMVar (pageDocument viewer) mayBeDoc


shouldDraw :: (Point, Point) -> (Point, Point) -> Bool
shouldDraw ((_x0, y0), (_w0, h0)) ((_x1, y1), (_w1, h1)) =
    (y0 >= y1 && y0 <= y1 + h1)
    || (y0 + h0 >= y1 && y0 + h0 <= y1 + h1)
