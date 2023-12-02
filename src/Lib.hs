module Lib
  ( parseGames,
    splitOn,
  )
where

import Data.Char (isDigit)

type ColorCount = (Int, Int, Int) -- (Red, Green, Blue)

type Game = (Int, [ColorCount]) -- (Game ID, Rounds)

splitOn :: Char -> String -> [String]
splitOn delimiter = foldr f [[]]
  where
    f c l@(x : xs)
      | c == delimiter = [] : l
      | otherwise = (c : x) : xs

parseGames :: String -> [Game]
parseGames content = map parseGame $ lines content
  where
    parseGame line =
      let (gameIdStr : roundsStr) = splitOn ':' line
          gameId = read $ filter isDigit gameIdStr
          rounds = map parseColorCount $ concatMap (splitOn ',') $ splitOn ';' $ head roundsStr
       in (gameId, rounds)

    parseColorCount :: String -> ColorCount
    parseColorCount colorStr =
      let (countStr : color : _) = words colorStr
          count = read countStr
          (r, g, b) = case color of
            "red" -> (count, 0, 0)
            "green" -> (0, count, 0)
            "blue" -> (0, 0, count)
            _ -> (0, 0, 0)
       in (r, g, b)
