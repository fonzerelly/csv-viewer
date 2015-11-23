import CsvViewer.Core
import CsvViewer.Keybindings

import System.Environment
import System.Console.Terminal.Size
import System.Console.ANSI
import Data.Maybe
import Data.List

main :: IO ()
main = do
   params <- getArgs
   let csvFile = head params
   contents <- readFile csvFile
   dispatchUserInput contents 0


truncateOutput :: String -> (Int, Int) -> Int -> String
truncateOutput contents console offset = unlines truncatedRows
   where rows = transformTable $ splitIntoRows contents
         visibleRows = map (intercalate "|") $ take ((snd console)-1) $ drop offset rows
         truncatedRows = map (take ((fst console)-1)) visibleRows

dispatchUserInput :: String -> Int-> IO ()
dispatchUserInput contents firstRow = do
   s <- size
   let console = (width $ fromJust s, height $ fromJust s)
       handleKey (-1) _ = 0
       handleKey offset UP
         | offset == 0 = 0
         | otherwise = offset - 1
       handleKey offset DOWN
         | offset >= (length contents) - (snd console) = (length contents) - (snd console)
         | otherwise = offset + 1
   putStrLn $ truncateOutput contents console firstRow
   putStrLn $ show firstRow
   c <- catchKey
   if c == QUIT
      then return ()
      else do
         setCursorPosition 0 0
         dispatchUserInput contents (handleKey firstRow c)



