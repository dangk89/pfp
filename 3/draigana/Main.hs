module Main where

import Dragon

main :: IO ()
main = do
  incomplete <- readIncomplete
  let m = nextMove incomplete
  printMove m
