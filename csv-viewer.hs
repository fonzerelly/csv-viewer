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
   dispatchUserInput (transformTable (splitIntoRows contents)) (0, 0)

dispatchUserInput :: [Row] -> CellOffset -> IO ()
dispatchUserInput contents offset = do
   s <- size
   let w = resize (fromJust s) 0 (-1)
   putStrLn $ truncateTable contents offset w
   dispatch contents offset w


dispatch :: [Row] -> CellOffset -> Window Int -> IO ()
dispatch contents offset w = do
   let calcVisibleCols index = length visibleRowPart + (sum (map length visibleRowPart))
         where visibleRowPart = init (drop index (head contents))

       handleKey offset UP
         | (snd offset) > 0 = ((fst offset), (snd offset) - 1)
         | otherwise = offset
       handleKey offset DOWN
         | (snd offset) < (length contents) - (height w) = ((fst offset), (snd offset) + 1)
         | otherwise = offset
       handleKey offset RIGHT
         | calcVisibleCols (fst offset) > (width w) = ((fst offset + 1), (snd offset))
         | otherwise = offset
       handleKey offset LEFT
         | (fst offset) > 0 = ((fst offset - 1), (snd offset))
         | otherwise = offset

   c <- catchKey
   if c == QUIT
      then do
         clearScreen
         return ()
      else do
         let newOffset = handleKey offset c
         if newOffset == offset
            then dispatch contents offset w
            else do
               setCursorPosition 0 0
               dispatchUserInput contents newOffset

