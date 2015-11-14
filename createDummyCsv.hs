import System.Environment
import System.Random
import System.Time
import Data.List
import CsvViewer.Types

main = do
   args <- getArgs
   now <- getClockTime
   clt <- toCalendarTime now

   let gen = mkStdGen $ fromInteger $ ctPicosec clt
       (cols, rows) = parseParams args

   putStrLn $ csvsifyTable $ createRandomTable gen rows cols

createRandomWord :: StdGen -> Int -> (String, StdGen)
createRandomWord gen range = (take n (randomRs ('A', 'z') g), g)
   where (n, g) = randomR (0,range) gen :: (Int, StdGen)

createRandomRow :: StdGen -> Int -> (Row, StdGen)
createRandomRow gen maxLength = randomList g n
   where (n, g) = randomR (0, maxLength) gen :: (Int, StdGen)
         randomList gl 0 = ([], gl)
         randomList gl n = (word : (fst $ randomList gw $ n-1), gw)
            where (word, gw) = createRandomWord gl 20

createRandomTable :: StdGen -> Int -> Int -> [Row]
createRandomTable _ 0 _ = []
createRandomTable gen rows cols = r : rs
   where (r, g) = createRandomRow gen cols
         rs = createRandomTable g (rows-1) cols

csvsifyTable :: [Row] -> String
csvsifyTable rs = unlines (map (intercalate ";") rs)

dummy ::  Int -> Int -> String
dummy c r = (show c) ++ (show r)

-- createTable :: Int ->) Int -> String
-- createTable c r = show $ createRow cDjkh--
-- createRow :: Int -> [Int])
-- createRow c = take c $ randoms $ mkStdGen c :: Int
--
parseParams :: [String] -> (Int, Int)
parseParams [] = error "Expects first cols then rows as arguments"
parseParams (c:[]) =  ((read c),  (read c))
parseParams (c:r:[]) = ((read c), (read r))
parseParams (_:_:args) = error "Too many arguments"
