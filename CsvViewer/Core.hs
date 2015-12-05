module CsvViewer.Core where

import Data.List
import Data.List.Split

import CsvViewer.Types
import System.Console.Terminal.Size

splitIntoRows :: String -> [Row]
splitIntoRows = map (splitOn ";") . lines

longest:: [[a]] -> Int
longest = (foldr (\item init -> max init $ length item) 0)

centerInRange :: Range -> String -> String
centerInRange r cell = replicate left ' ' ++ cell ++ replicate right ' '
   where
      l = length cell
      w = r - l
      half = (`div` 2)
      left = half w
      right
         | odd w =  half (w + 1)
         | otherwise = half w


maximizeRows :: [Row] -> [Row]
maximizeRows xs = maximize mR xs
   where mR = longest xs
         maximize _ [] = []
         maximize m (r:rs) = (r ++ replicate (m - (length r)) "") : (maximize m rs)

maximizeColumns :: [Col] -> [Col]
maximizeColumns [] = []
maximizeColumns (x:xs) = map (centerInRange mW) x : maximizeColumns xs
   where mW = longest x

transformTable :: [Row] -> [Row]
transformTable = transpose . maximizeColumns . maximizeRows . transpose

joinRowsByPipe :: [Row] -> String
joinRowsByPipe = unlines . (map join)
   where join = ((foldr (++) "") . (intersperse "|"))

displayCsv :: String -> String
displayCsv = joinRowsByPipe . transformTable. splitIntoRows

slice :: Int -> Int -> [a] ->  [a]
slice u l rs = take l $ drop u rs

truncateTable :: [Row] -> CellOffset -> Window Int -> String
truncateTable rs co w = unlines $ takeWidth $ stringifyRows $ skipRows $ skipCols rs
   where
      takeWidth = map (take (width w))
      stringifyRows = (map (intercalate "|"))
      skipCols = map (drop (fst co))
      skipRows = slice (snd co) (height w)

resize :: (Num a) => Window a -> a -> a -> Window a
resize win h w = Window ((height win) + h) ((width win) + w)
