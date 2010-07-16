{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

module LambdaCat.Page.Poppler 
    ( PopplerPage
    ) where

import LambdaCat.Page hiding (Page)

import Control.Concurrent
import Control.Monad.Trans
import Data.Typeable
import Graphics.UI.Gtk hiding (Point)
import Graphics.UI.Gtk.Poppler.Document hiding (PageClass)
import Graphics.UI.Gtk.Poppler.Page 
import Network.URI
import Data.Maybe
import Control.Monad
import Graphics.Rendering.Cairo


data PopplerPage = PopplerPage
    { pageArea     :: DrawingArea
    , pageScrollable :: ScrolledWindow
    , pageDocument :: MVar (Maybe Document)
    , pageNumber   :: MVar Int 
    , pageURI      :: MVar URI
    }
  deriving (Eq, Typeable)

type CurrentPage = Int
type Point = (Double,Double)
type PagePositioner = Document  -- ^ Document that should be layouted
                    -> CurrentPage -- ^ Page which must be visible 
                    -> Int -- ^ Number of pages shown
                    -> Point -- ^ Size of the Screen
                    -> IO [(Page,Double,Point,Point)] -- ^ Where should which page be drawn

instance HasWidget PopplerPage ScrolledWindow where
    getWidget = pageScrollable

instance MonadIO m => PageClass PopplerPage m where 
    new _ = liftIO $ do
        area <- drawingAreaNew
        scrollWindow <- scrolledWindowNew Nothing Nothing
        doc <- newMVar Nothing
        num <- newMVar 0
        uri <- newMVar nullURI

        scrolledWindowAddWithViewport scrollWindow area
        scrolledWindowSetPolicy scrollWindow PolicyAutomatic PolicyAutomatic

        let popplerPage = PopplerPage { pageArea = area
                                      , pageScrollable = scrollWindow
                                      , pageDocument = doc
                                      , pageNumber = num 
                                      , pageURI = uri
                                      }
        
        _ <- area `on` exposeEvent $ tryEvent $ viewerDraw popplerPage

        return popplerPage

    load PopplerPage { pageArea = area,  pageDocument = mdoc } uri = liftIO $ do
        doc <- liftM (fromMaybe (error "Error opening pdf file.")) (documentNewFromFile uriString Nothing)
        _ <-  takeMVar mdoc
        putMVar mdoc (Just doc)
        widgetQueueDraw area 
     where uriString = uriToString id uri ""

    getCurrentURI page = do 
        let mURI = (pageURI page)
        liftIO $ withMVar mURI return
 
    getCurrentTitle page = do 
        let mDoc = (pageDocument page)
        liftIO $ withMVar mDoc $ \ m -> case m of
                (Just d) -> get d documentTitle
                _ -> return ""

viewerDraw :: PopplerPage -> EventM EExpose ()
viewerDraw viewer = do
  let area = pageArea viewer
      scroll = pageScrollable viewer
  pN       <- liftIO $ readMVar $ pageNumber viewer
  mayBeDoc <- liftIO $ takeMVar $ pageDocument viewer-- TODO replace with takeMVar
  (winWidth, winHeight) <- liftIO $ widgetGetSize scroll
  case mayBeDoc of
    Nothing -> return ()
    (Just doc) -> liftIO $ do
        frameWin  <- widgetGetDrawWindow area 

        positions <- fitPage doc pN 2 (fromIntegral winWidth,fromIntegral winHeight)
        let widgetWidth = foldr (\ (_,_,(x0,_),(w,_)) m -> m `max` (x0 + w)) 0 positions
        let widgetHeight = foldr (\ (_,_,(_,y0),(_,h)) m -> m `max` (y0 + h)) 0 positions
        widgetSetSizeRequest area (truncate widgetWidth) (truncate widgetHeight)

        mapM_ (\ (page,scal,(x0,y0),(x1,y1)) -> renderWithDrawable frameWin $ do 
            setSourceRGB 1.0 1.0 1.0
            rectangle x0 y0 x1 y1 
            fill
            translate x0 y0
            scale scal scal
            pageRender page
            ) positions
  liftIO $ putMVar (pageDocument viewer) mayBeDoc


fitPage :: PagePositioner 
fitPage doc currentPage count (winWidth,winHeight) = do
    numOfPages <- documentGetNPages doc
    let cPosNum   = currentPage `mod` count
        firstPage = currentPage - cPosNum
        lastPage  = numOfPages `min` firstPage + count - 1 
        pageSpace = fromIntegral $ floor $ winWidth / fromIntegral count :: Double 
    pages <- mapM (documentGetPage doc) [firstPage..lastPage]
    foldM (\ lst page -> do
            (pageWidth,pageHeight) <- pageGetSize page
            let leftSide  = fromIntegral (length lst )  * pageSpace
                rightSide = leftSide + pageSpace `min` winWidth
                drawHeight = if leftSide + (winHeight / pageHeight) * pageWidth > rightSide
                             then (pageSpace / pageWidth) * pageHeight
                             else winHeight - 5
                scaleX = drawHeight / pageHeight
                drawWidth = pageWidth * scaleX
                x0 = fromIntegral $  0 `max` floor ( (pageSpace - drawWidth) / 2)
                y0 = fromIntegral $  0 `max` floor ( (winHeight - drawHeight) / 2)
            return $ (page,scaleX,(x0 + leftSide,y0),(drawWidth,drawHeight)):lst
         ) [] pages
