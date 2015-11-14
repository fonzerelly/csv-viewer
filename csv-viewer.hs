import CsvViewer.Core
import System.Environment
import System.Console.Terminal.Size
import Data.Maybe
import Data.List

main = do
   params <- getArgs
   s <- size
   let csvFile = head params
       consoleWidth = width $ fromJust s
       consoleHeight = height $ fromJust s
   contents <- readFile csvFile

   let rows = transformTable $ splitIntoRows contents
       visibleRows = map (intercalate "|") $ take consoleHeight rows
       truncatedRows = map (take (consoleWidth-1)) visibleRows
       screen =  unlines truncatedRows

   putStrLn screen
