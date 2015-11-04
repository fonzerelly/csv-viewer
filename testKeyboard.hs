{-# LANGUAGE ForeignFunctionInterface #-}
import Data.Char
import Control.Monad (liftM)
import Foreign.C.Types
import System.Console.Terminal.Size
import Data.Maybe
import Data.Char
import Unsafe.Coerce

getHiddenChar = liftM (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt

catchKey :: IO CInt
catchKey = do
   c <- c_getch
   if c /= 224
      then  return c
      else do
         c <- c_getch
         return c

-- Find a way to ensure that CInt stays inside Int
displayChar :: CInt -> String
displayChar 72 = "Up"
displayChar 80 = "Down"
displayChar 75 = "Left"
displayChar 77 = "Right"
displayChar c = [chr (fromIntegral c)]


main = do
   s <-  size
   let consoleWidth = width $ fromJust s
       consoleHeight = height $ fromJust s
       screen = unlines $ replicate consoleHeight $ replicate consoleWidth '1'
   c <- catchKey
   putStrLn (displayChar c)
   if c `elem` [3,4,27]
      then return ()
      else main
