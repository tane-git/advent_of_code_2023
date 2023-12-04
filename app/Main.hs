module Main where

import Lib (sumPossibleGamesIds)

main :: IO ()
main = do
    content <- readFile "day2-input.txt"

    let games = sumPossibleGamesIds content

    print games
