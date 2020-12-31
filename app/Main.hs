module Main where

import Lib
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  let filepath = head args
  handle <- openFile filepath ReadMode
  dimacs <- hGetContents handle
  let res = sat $ parseDimacs dimacs
  print res
