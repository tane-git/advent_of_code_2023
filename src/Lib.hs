module Lib ( parseGames, parseGame, parseRound, parseCubes , CubeCount, Game) where

import Data.List.Split (splitOn)

type CubeCount = (Int, Int, Int) -- (Red, Green, Blue)
type Game = (Int, [CubeCount]) -- (GameID, Rounds)

parseGames :: String -> [Game]
parseGames content = map parseGame $ lines content

parseGame :: String -> Game
parseGame gameLine =
  let (gameIdStr : rounds) = splitOn ": " gameLine
      gameId = read $ drop 5 gameIdStr
      parsedRounds = map parseRound rounds
   in (gameId, concat parsedRounds)

parseRound :: String -> [CubeCount]
parseRound roundsStr =
  let rounds = splitOn "; " roundsStr
   in map parseCubes rounds

parseCubes :: String -> CubeCount
parseCubes cubesStr =
  let cubes = splitOn ", " cubesStr
      parseCube cube = read . head . drop 1 . splitOn " " $ cube
      reds = sum $ map parseCube $ filter (elem 'r') cubes
      greens = sum $ map parseCube $ filter (elem 'g') cubes
      blues = sum $ map parseCube $ filter (elem 'b') cubes
   in (reds, greens, blues)

