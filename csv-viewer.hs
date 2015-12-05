import CsvViewer.Core
import CsvViewer.Types
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
   dispatchUserInput (transformTable (splitIntoRows contents)) 0

dispatchUserInput :: [Row] -> Int-> IO ()
dispatchUserInput contents firstRow = do
   s <- size
   let console = (width $ fromJust s, height $ fromJust s)
       w = resize (fromJust s) 0 (-1)
       handleKey (-1) _ = 0
       handleKey offset UP
         | offset == 0 = 0
         | otherwise = offset - 1
       handleKey offset DOWN
         | offset >= (length contents) - (snd console) = (length contents) - (snd console)
         | otherwise = offset + 1
   putStrLn $ truncateTable contents (0, firstRow) w
   c <- catchKey
   if c == QUIT
      then return ()
      else do
         setCursorPosition 0 0
         dispatchUserInput contents (handleKey firstRow c)

