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
       handleKey (-1) _ = 0
       handleKey offset UP
         | offset == 0 = 0
         | otherwise = offset - 1
       handleKey offset DOWN
         | offset >= (length contents) - (height w) = (length contents) - (height w)
         | otherwise = offset + 1
   putStrLn $ truncateTable contents offset w
   c <- catchKey
   if c == QUIT
      then return ()
      else do
         setCursorPosition 0 0
         dispatchUserInput contents (0, (handleKey (snd offset) c))

