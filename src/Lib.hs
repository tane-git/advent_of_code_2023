module Lib (sumPowers, minimumCubesAndPower, sumPossibleGamesIds, parseGames, parseGame, parseRound, parseCubes , CubeCount, Game) where

import Data.List.Split (splitOn)
import Data.List (find, isInfixOf)

type CubeCount = (Int, Int, Int) -- (Red, Green, Blue)
type Game = (Int, [CubeCount]) -- (GameID, Rounds)

sumPowers :: String -> Int
sumPowers content = minimumCubesAndPower $ parseGames content

minimumCubesAndPower :: [Game] -> Int
minimumCubesAndPower games = sum $ map (power . minCubes) games
  where
    minCubes :: Game -> CubeCount
    minCubes (_, rounds) = foldl1 maxCubes rounds

    maxCubes :: CubeCount -> CubeCount -> CubeCount
    maxCubes (r1, g1, b1) (r2, g2, b2) = (max r1 r2, max g1 g2, max b1 b2)

    power :: CubeCount -> Int
    power (r, g, b) = r * g * b

sumPossibleGamesIds :: String -> Int
sumPossibleGamesIds content =
  let games = parseGames content
      possibleGames = filter (isGamePossible 12 13 14) games
   in sum $ map fst possibleGames

isGamePossible :: Int -> Int -> Int -> Game -> Bool
isGamePossible maxReds maxGreens maxBlues (gameId, rounds) =
  all (\(reds, greens, blues) -> reds <= maxReds && greens <= maxGreens && blues <= maxBlues) rounds


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
      findCount color = 
        case find (isInfixOf color) cubes of
          Just cube -> read . head . splitOn " " $ cube
          Nothing   -> 0
      reds = findCount "red"
      greens = findCount "green"
      blues = findCount "blue"
  in (reds, greens, blues)
