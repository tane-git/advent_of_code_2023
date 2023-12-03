module Main where

import Lib (parseGames)
import System.IO (readFile)

main :: IO ()
main = do
    content <- readFile "day2-input.txt"

    let games = parseGames content

    print games