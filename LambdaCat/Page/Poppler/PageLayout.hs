module LambdaCat.Page.Poppler.PageLayout
    ( DocumentGeometry
    , PageLayout
    , PageLayoutAlgorithm

    , toDocumentGeometry
    , fromPageLayout

    , maxDocumentSize
    , maxPagesSize
    , documentGetPage
    , documentGetNPages
    , pageLayoutSize

    , fitPage
    , continue

    ) where

import Control.Arrow
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Graphics.UI.Gtk.Poppler.Document as PopplerDocument hiding (PageClass, PageLayout)
import qualified Graphics.UI.Gtk.Poppler.Page as PopplerPage


type DocumentGeometry = Map Int (Double, Double) -- Width, Height of PopplerPage with number Int
type PageLayout = (Int, Double, (Double, Double), (Double, Double))
type PageLayoutAlgorithm = Int -> (Double, Double) -> DocumentGeometry -> [PageLayout]

toDocumentGeometry :: PopplerDocument.Document -> IO DocumentGeometry
toDocumentGeometry doc = do
    numOfPages <- PopplerDocument.documentGetNPages doc
    pages <- mapM (\ i -> do
                   page <-  PopplerDocument.documentGetPage doc i
                   return (i, page)
                  ) [0 .. numOfPages - 1]
    foldM (\ m (i, page) -> do
            dim <- PopplerPage.pageGetSize page
            return $ Map.insert i dim m
          ) Map.empty pages

fromPageLayout
    :: [PageLayout]
    -> PopplerDocument.Document
    -> IO [(PopplerDocument.Page, Double, (Double, Double), (Double, Double))]
fromPageLayout lst doc = mapM (\ (nr, a, b, c) -> do
        page <- PopplerDocument.documentGetPage doc nr
        return (page, a, b, c)
    ) lst

pageLayoutSize :: [PageLayout] -> (Double, Double)
pageLayoutSize = foldr (\ (_, _, (x0, y0), (w, h)) (m, n) -> (m `max` (x0 + w), n `max` (y0 + h))) (0, 0)

documentSize
    :: (Double -> Double -> Double)
    -> DocumentGeometry
    -> (Double, Double)
documentSize s m =
    let fd = \ f -> Map.fold f 0 m
    in (fd (s . fst), fd (s . snd))

maxDocumentSize
    :: DocumentGeometry
    -> (Double, Double)
maxDocumentSize = documentSize max

maxPagesSize
    :: [Int]            -- ^ pages
    -> DocumentGeometry -- ^ document geometry
    -> (Double, Double)  -- ^ max height/width of pages
maxPagesSize pages m = let fMap = Map.filterWithKey (\ k _ -> k `elem` pages) m
                       in  maxDocumentSize fMap

documentGetNPages :: DocumentGeometry -> Int
documentGetNPages = Map.size

documentGetPage :: Int -> DocumentGeometry -> Maybe (Double, Double)
documentGetPage = Map.lookup

documentGetExistencePages :: [Int] -> DocumentGeometry -> [(Int, (Double, Double))]
documentGetExistencePages pages doc =
    map (second fromJust) . filter (isJust . snd)
    . map (\ i -> (i, documentGetPage i doc)) $ pages

fitPage :: Int -> PageLayoutAlgorithm
fitPage count current (winWidth, winHeight) doc =
    let fP = current - (current `mod` count)
        lP  = documentGetNPages doc `min` fP + count - 1
        pageSpace = winWidth / fromIntegral count
        pages     =  documentGetExistencePages [fP .. lP] doc
    in foldr (\ (nr, (pageWidth, pageHeight)) lst ->
            let leftSide  = fromIntegral (length lst )  * pageSpace
                rightSide = leftSide + pageSpace `min` winWidth
                selectHeight = leftSide + (winHeight / pageHeight) * pageWidth > rightSide
                drawHeight = if selectHeight then (pageSpace / pageWidth) * pageHeight
                                             else winHeight - 5
                scaleX = drawHeight / pageHeight
                drawWidth = pageWidth * scaleX
                x0 = 0 `max` ((pageSpace - drawWidth) / 2)
                y0 = 0 `max` ((winHeight - drawHeight) / 2)
            in (nr, scaleX, (x0 + leftSide, y0), (drawWidth, drawHeight)) : lst
         ) [] pages

continue :: PageLayoutAlgorithm -> PageLayoutAlgorithm
continue f current size doc =
    let normalFit   = reverse $ f current size doc
        numOfPages  = documentGetNPages doc
        pages       = documentGetExistencePages [0 .. numOfPages - 1] doc
        (_, height) = maxPagesSize (map (\ (n, _, _, _) -> n) normalFit) doc
    in foldl (\ lst (nr, (_, _h)) ->
            let crt       = length lst `mod` length normalFit
                iteration = length lst `div` length normalFit
                upperLine = (height + 50) * fromIntegral iteration
                (_, sc, (x0, y0), s0) = normalFit !! crt
                -- sc = height / h
            in (nr, sc, (x0, y0 + upperLine), s0) : lst
          ) [] pages

