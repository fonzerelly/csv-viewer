module CsvViewer.Keybindings where

import Data.Char
import Control.Monad (liftM)
import Foreign.C.Types

data Keybinding = UP | DOWN | LEFT | RIGHT | QUIT deriving (Eq, Show)

getHiddenChar = liftM (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt


catchKey :: IO Keybinding
catchKey = do
   c <- skipCtrlKeys
   return (convertKey c)

convertKey :: CInt -> Keybinding
convertKey 72 = UP
convertKey 80 = DOWN
convertKey 75 = LEFT
convertKey 77 = RIGHT
convertKey _ = QUIT

skipCtrlKeys:: IO CInt
skipCtrlKeys = do
   c <- c_getch
   if c /= 224
      then  return c
      else do
         c <- c_getch
         return c

