module Lib where

import Data.List.Split (splitOn)

parseGames :: String -> [(Int, [(Int, Int, Int)])]
parseGames content = map parseGame $ lines content

parseGame :: String -> (Int, [(Int, Int, Int)])
parseGame gameLine =
  let (gameIdStr : rounds) = splitOn ": " gameLine
      gameId = read $ drop 5 gameIdStr
      parsedRounds = map parseRound rounds
   in (gameId, concat parsedRounds)

parseRound :: String -> [(Int, Int, Int)]
parseRound roundsStr =
  let rounds = splitOn "; " roundsStr
   in map parseCubes rounds

parseCubes :: String -> (Int, Int, Int)
parseCubes cubesStr =
  let cubes = splitOn ", " cubesStr
      parseCube cube = read . head . drop 1 . splitOn " " $ cube
      reds = sum $ map parseCube $ filter (elem 'r') cubes
      greens = sum $ map parseCube $ filter (elem 'g') cubes
      blues = sum $ map parseCube $ filter (elem 'b') cubes
   in (reds, greens, blues)
