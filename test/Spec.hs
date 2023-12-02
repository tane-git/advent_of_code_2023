import Test.Hspec
import Lib (parseGames)
import System.IO (readFile)

main :: IO ()
main = hspec $ do
  describe "parseGames" $ parseGamesTest

parseGamesTest :: Spec
parseGamesTest = do
  it "reads and parses file content into a list of games" $ do
    content <- readFile "day2-test-input.txt"
    let expected = [
          (1, [(4, 0, 3), (1, 2, 6), (0, 2, 0)]),
          (2, [(0, 2, 1), (4, 3, 1), (0, 1, 1)])
        ]
    parseGames content `shouldBe` expected
