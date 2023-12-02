module Main where

import Lib (concatFirstLastNumbers, findFirstNumber, findLastNumber)

main :: IO ()
main = do
  content <- readFile "input.txt"

  print content

  let linesOfNumbers = map concatFirstLastNumbers (lines content)

  print linesOfNumbers

  let total = sum linesOfNumbers

  print total
