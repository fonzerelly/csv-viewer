import CsvViewer.Core
import System.Environment

main = do
   params <- getArgs
   let csvFile = head params
   contents <- readFile csvFile
   putStrLn (displayCsv contents)
