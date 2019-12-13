module Main where

import Three
import System.IO
import Control.Monad

main :: IO ()
main = do
  handle <- openFile "inputs/three.txt" ReadMode
  contents <- hGetContents handle
  let answer = findAnswer contents
  print answer
