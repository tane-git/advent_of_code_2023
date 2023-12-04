module Main where

import Lib (concatFirstLastNumbers, findFirstNumber, findLastNumber)

main :: IO ()
main = do
  content <- readFile "input.txt"

  let linesOfNumbers = map concatFirstLastNumbers (lines content)

  let total = sum linesOfNumbers

  print total
