{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( tabsToSpaces
    --, spacesToTabs
    ) where
import           Control.Arrow        ((&&&), (***), (>>>))
import           Data.Foldable        (fold)
import           Data.Functor.Compose
import           Data.List
import           Data.Maybe           (catMaybes, isJust)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Safe                 (atMay)

groupAdjacentBy :: (a -> Bool) -> [a] -> [[a]]
groupAdjacentBy f = groupBy (curry (f *** f >>> uncurry (&&)))

maxAdjacentBy :: Ord a => (a -> Bool) -> [a] -> [[a]]
maxAdjacentBy = fmap (length &&& maximum >>> uncurry replicate) . groupAdjacentBy

maxedWidthsPerLine :: [[Int]] -> [[Int]]
maxedWidthsPerLine textWidths = catMaybes <$> transpose maxedWidthsPerColumn
  where
    maxNofCells = maximum $ fmap length textWidths
    maxedWidthsPerColumn =
      [ widths | i <- [0..maxNofCells],
                 let widths = concat $ maxAdjacentBy isJust $ flip atMay i . dropRight 1 <$> textWidths ]
    dropRight n = reverse . drop n . reverse

tabsToSpaces :: Text -> Int -> Text
tabsToSpaces text nofIndent = T.intercalate "\n" $ zipWith withSpaces cellsPerLine maxedWidths
  where
    cellPadding = 2
    cellMinimum = nofIndent - cellPadding
    cellsPerLine = T.splitOn "\t" <$> T.splitOn "\n" text
    maxedWidths = maxedWidthsPerLine $ getCompose $ cellWidthOf <$> Compose cellsPerLine
    cellWidthOf text = max (T.length text) cellMinimum + cellPadding
    lineWithSpaces cell width = cell <> T.replicate (width - T.length cell) " "
    withSpaces widths cells = fold $ zipWith lineWithSpaces widths (cells ++ [0])

