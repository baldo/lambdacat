module LambdaCat.Page.Poppler.PageLayout 
    ( DocumentGeometry
    , PageLayout
    , PageLayoutAlgorithm

    , toDocumentGeometry

    , maxDocumentSize
    , maxPagesSize

    , documentGetNPages

    ) where

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Graphics.UI.Gtk.Poppler.Document as PopplerDocument hiding (PageClass,PageLayout) 
import qualified Graphics.UI.Gtk.Poppler.Page as PopplerPage


type DocumentGeometry = Map Int (Double,Double) -- Width,Height of PopplerPage with number Int 
type PageLayout = (Int,Double,(Double,Double),(Double,Double))
type PageLayoutAlgorithm = DocumentGeometry -> Int -> Int -> (Double,Double) -> [PageLayout]

toDocumentGeometry :: PopplerDocument.Document -> IO DocumentGeometry
toDocumentGeometry doc = do
    numOfPages <- PopplerDocument.documentGetNPages doc
    pages <- mapM (\ i -> do
                   page <-  PopplerDocument.documentGetPage doc i
                   return (i,page)
                  ) [0..numOfPages - 1]
    foldM (\ map (i,page) -> do
            dim <- PopplerPage.pageGetSize page 
            return $ Map.insert i dim map
          ) Map.empty pages

documentSize
    :: (Double -> Double -> Double)
    -> DocumentGeometry 
    -> (Double,Double)
documentSize s map = 
    let fd = \f -> Map.fold f 0 map
    in (fd (s.fst), fd (s.snd))

maxDocumentSize
    :: DocumentGeometry 
    -> (Double,Double)
maxDocumentSize = documentSize max

maxPagesSize
    :: [Int]            -- ^ pages
    -> DocumentGeometry -- ^ document geometry 
    -> (Double,Double)  -- ^ max height/width of pages
maxPagesSize pages map = let fMap = Map.filterWithKey (\ k _ -> k `elem` pages) map
                         in  maxDocumentSize fMap

documentGetNPages :: DocumentGeometry -> Int
documentGetNPages = Map.size  
