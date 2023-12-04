module Main where

import Lib (sumPossibleGamesIds, sumPowers)

main :: IO ()
main = do
    content <- readFile "day2-input.txt"

    let sumOfPossibleGamesIds = sumPossibleGamesIds content

    print sumOfPossibleGamesIds

    let sumOfPowers = sumPowers content

    print sumOfPowers
