import System.Environment
import System.Random
import System.Time

main = do
   args <- getArgs
   now <- getClockTime
   clt <- toCalendarTime now

   putStrLn $ show $ ctPicosec clt
   let gen = mkStdGen $ fromInteger $ ctPicosec clt
       (cols, rows) = parseParams args
       -- rnd = randoms gen :: [Int]

   putStrLn $ show $ fst $ createRandomRow gen cols
   -- putStrLn $ show (take 5 rnd)
   -- putStrLn $ show (take 5 (drop 5 rnd))
   -- let (cols, rows) = parseParams args
   -- putStrLn $ dummy cols rows
   --
   --
createRandomWord :: StdGen -> Int -> (String, StdGen)
createRandomWord gen range = (take n (randomRs ('A', 'z') g), g)
   where (n, g) = randomR (0,range) gen :: (Int, StdGen)

createRandomRow :: StdGen -> Int -> ([String], StdGen)
createRandomRow gen maxLength = randomList g n
   where (n, g) = randomR (0, maxLength) gen :: (Int, StdGen)
         randomList gl 0 = ([], gl)
         randomList gl n = (word : (fst $ randomList gw $ n-1), gw)
            where (word, gw) = createRandomWord gl 20

-- createRandomTable :: StdGen -> Int -> Int -> String
-- createRandomTable gen cols rows = unlines

dummy ::  Int -> Int -> String
dummy c r = (show c) ++ (show r)

-- createTable :: Int -> Int -> String
-- createTable c r = show $ createRow c
--
-- createRow :: Int -> [Int]
-- createRow c = take c $ randoms $ mkStdGen c :: Int
--
parseParams :: [String] -> (Int, Int)
parseParams [] = error "Expects first cols then rows as arguments"
parseParams (c:[]) =  ((read c),  (read c))
parseParams (c:r:[]) = ((read c), (read r))
parseParams (_:_:args) = error "Too many arguments"
