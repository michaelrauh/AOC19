module Main where

import Lib
import System.IO  
import Control.Monad

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  putStrLn contents
