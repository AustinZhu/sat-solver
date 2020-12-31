module Main where

import Lib
import System.IO

main :: IO [()]
main = do
  let filepath = map path [1 .. 1000]
  mapM run filepath
  where
    path a = "./problems/uf20-0" ++ show a ++ ".cnf"
    run p = do
      handle <- openFile p ReadMode
      dimacs <- hGetContents handle
      let res = sat $ parseDimacs dimacs
      print res
