import Lib (parseGames, parseGame, parseRound, parseCubes, Game)
import System.IO (readFile)
import Test.HUnit
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let selectedTests = case args of
        ["parseGames"] -> parseGamesTests
        ["parseGame"] -> parseGameTests
        ["parseRound"] -> parseRoundTests
        ["parseCubes"] -> parseCubesTests
        _ -> allTests -- default to running all tests
  runTestTT selectedTests
  return ()

allTests :: Test
allTests = TestList 
  [ parseGamesTests,
    parseGameTests,
    parseRoundTests,
    parseCubesTests
  ]


-- parseGames
parseGamesTests :: Test
parseGamesTests = TestList 
  [ TestLabel "Test for parseGames" testParseGames ]

expectedGames :: [Game]
expectedGames =
  [ (1, [(4, 0, 3), (1, 2, 6), (0, 2, 0)]),
    (2, [(0, 2, 1), (1, 3, 4), (0, 1, 1), (1, 1, 1)]),
    (3, [(20, 8, 6), (4, 13, 5), (1, 5, 0)]),
    (4, [(3, 1, 6), (6, 3, 0), (14, 3, 15)]),
    (50, [(6, 3, 1), (1, 2, 2)])
  ]

testParseGames :: Test
testParseGames = TestCase $ do
  content <- readFile "day2-test-input.txt"
  parseGames content @?= expectedGames


-- parseGame
parseGameTests :: Test
parseGameTests = TestList 
  [ TestLabel "Test for parseGame" testParseGame ]

inputGame = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
expectedGame = (1, [(4, 0, 3), (1, 2, 6), (0, 2, 0)])

testParseGame :: Test
testParseGame = TestCase $ do
  parseGame inputGame @?= expectedGame


-- parseRound
parseRoundTests :: Test
parseRoundTests = TestList 
  [ TestLabel "Test for parseRound" testParseRound ]

testParseRound :: Test
testParseRound = TestCase $ do
  let input = "3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
  let expected = [(4, 0, 3), (1, 2, 6), (0, 2, 0)]
  parseRound input @?= expected


-- parseCubes
parseCubesTests :: Test
parseCubesTests = TestList 
  [ TestLabel "Test for parseCubes" testParseCubes ]

testParseCubes :: Test
testParseCubes = TestCase $ do
  assertEqual "for (3 blue, 4 red)," (4, 0, 3) (parseCubes "3 blue, 4 red")
  assertEqual "for (1 red, 2 green, 6 blue)," (1, 2, 6) (parseCubes "1 red, 2 green, 6 blue")
  assertEqual "for (2 green)," (0, 2, 0) (parseCubes "2 green")
