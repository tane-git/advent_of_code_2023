import Lib (parseGames)
import System.IO (readFile)
import Test.HUnit

main :: IO ()
main = do
  runTestTT parseGamesTests
  return ()

parseGamesTests :: Test
parseGamesTests = TestList [TestLabel "Test for parseGames" testParseGames]

expectedGames :: [(Int, [(Int, Int, Int)])]
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
