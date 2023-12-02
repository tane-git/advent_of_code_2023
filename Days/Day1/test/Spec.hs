import Lib (concatFirstLastNumbers, findFirstNumber, findLastNumber)
-- import Test.HUnit
--   ( Counts,
--     Test (TestCase, TestList),
--     assertEqual,
--     runTestTT,
--   )

-- testFindFirstNumber :: Test
-- testFindFirstNumber = TestList $ map createTest cases
--   where
--     cases =
--       [ ("two1nine", Just 2),
--         ("eightwothree", Just 8),
--         ("abcone2threexyz", Just 1),
--         ("xtwone3four", Just 2),
--         ("4nineeightseven2", Just 4),
--         ("zoneight234", Just 1),
--         ("7pqrstsixteen", Just 7),
--         ("1abc2", Just 1),
--         ("pqr3stu8vwx", Just 3),
--         ("a1b2c3d4e5f", Just 1),
--         ("treb7uchet", Just 7),
--         ("xjpdjzdn5dltxnvqtkknvxsnqsmn7bzlgp7hdhbvn", Just 5),
--         ("", Nothing),
--         ("oneabcnine", Just 1),
--         ("12345", Just 1),
--         ("onetwothreefourfive", Just 1),
--         ("x", Nothing),
--         ("7", Just 7),
--         ("three", Just 3),
--         ("abc3def2ghi", Just 3),
--         ("2two2two", Just 2),
--         ("this is a long string with 4 and nine in it", Just 4),
--         ("onet2", Just 1),
--         ("tone2", Just 1),
--         ("tonep2heighto", Just 1),
--         ("twofold1three", Just 2),
--         ("ninexzhtqsr6hnftrbbnsevensevenoneightq", Just 9)
--       ]
--     createTest (input, expected) = TestCase (assertEqual ("for input: " ++ input) expected (findFirstNumber input))

-- testFindLastNumber :: Test
-- testFindLastNumber = TestList $ map createTest cases
--   where
--     cases =
--       [ ("two1nine", Just 9),
--         ("eightwothree", Just 3),
--         ("abcone2threexyz", Just 3),
--         ("xtwone3four", Just 4),
--         ("4nineeightseven2", Just 2),
--         ("zoneight234", Just 4),
--         ("7pqrstsixteen", Just 6),
--         ("1abc2", Just 2),
--         ("pqr3stu8vwx',", Just 8),
--         ("a1b2c3d4e5f", Just 5),
--         ("treb7uchet", Just 7),
--         ("xjpdjzdn5dltxnvqtkknvxsnqsmn7bzlgp7hdhbvn", Just 7),
--         ("oneabcnine", Just 9),
--         ("12345", Just 5),
--         ("onetwothreefourfive", Just 5),
--         ("7", Just 7),
--         ("three", Just 3),
--         ("abc3def2ghi", Just 2),
--         ("2two2two", Just 2),
--         ("this Just is a long string with 4 and nine in it", Just 9),
--         ("onet2", Just 2),
--         ("tone2", Just 2),
--         ("tonep2heighto", Just 8),
--         ("twofold1three", Just 3),
--         ("ninexzhtqsr6hnftrbbnsevensevenoneightq", Just 8)
--       ]
--     createTest (input, expected) = TestCase (assertEqual ("for input: " ++ input) expected (findLastNumber input))

-- testConcatFirstLastSpelledNumbers :: Test
-- testConcatFirstLastSpelledNumbers =
--   TestList
--     [ TestCase (assertEqual "for concatFirstLastNumbers 'two1nine'," 29 (concatFirstLastNumbers "two1nine")),
--       TestCase (assertEqual "for concatFirstLastNumbers 'eightwothree'," 83 (concatFirstLastNumbers "eightwothree")),
--       TestCase (assertEqual "for concatFirstLastNumbers 'abcone2threexyz'," 13 (concatFirstLastNumbers "abcone2threexyz")),
--       TestCase (assertEqual "for concatFirstLastNumbers 'xtwone3four'," 24 (concatFirstLastNumbers "xtwone3four")),
--       TestCase (assertEqual "for concatFirstLastNumbers '4nineeightseven2'," 42 (concatFirstLastNumbers "4nineeightseven2")),
--       TestCase (assertEqual "for concatFirstLastNumbers 'zoneight234'," 14 (concatFirstLastNumbers "zoneight234")),
--       TestCase (assertEqual "for concatFirstLastNumbers '7pqrstsixteen'," 76 (concatFirstLastNumbers "7pqrstsixteen")),
--       TestCase (assertEqual "for concatFirstLastNumbers '1abc2'," 12 (concatFirstLastNumbers "1abc2")),
--       TestCase (assertEqual "for concatFirstLastNumbers 'pqr3stu8vwx'," 38 (concatFirstLastNumbers "pqr3stu8vwx")),
--       TestCase (assertEqual "for concatFirstLastNumbers 'a1b2c3d4e5f'," 15 (concatFirstLastNumbers "a1b2c3d4e5f")),
--       TestCase (assertEqual "for concatFirstLastNumbers 'treb7uchet'," 77 (concatFirstLastNumbers "treb7uchet")),
--       TestCase (assertEqual "for concatFirstLastNumbers 'xjpdjzdn5dltxnvqtkknvxsnqsmn7bzlgp7hdhbvn'," 57 (concatFirstLastNumbers "xjpdjzdn5dltxnvqtkknvxsnqsmn7bzlgp7hdhbvn")),
--       TestCase (assertEqual "for concatFirstLastNumbers '', no numbers" 0 (concatFirstLastNumbers "")),
--       TestCase (assertEqual "for concatFirstLastNumbers 'oneabcnine'," 19 (concatFirstLastNumbers "oneabcnine")),
--       TestCase (assertEqual "for concatFirstLastNumbers '12345'," 15 (concatFirstLastNumbers "12345")),
--       TestCase (assertEqual "for concatFirstLastNumbers 'onetwothreefourfive'," 15 (concatFirstLastNumbers "onetwothreefourfive")),
--       TestCase (assertEqual "for concatFirstLastNumbers 'x'," 0 (concatFirstLastNumbers "x")),
--       TestCase (assertEqual "for concatFirstLastNumbers '7'," 77 (concatFirstLastNumbers "7")),
--       TestCase (assertEqual "for concatFirstLastNumbers 'three'," 33 (concatFirstLastNumbers "three")),
--       TestCase (assertEqual "for concatFirstLastNumbers 'abc3def2ghi'," 32 (concatFirstLastNumbers "abc3def2ghi")),
--       TestCase (assertEqual "for concatFirstLastNumbers '2two2two'," 22 (concatFirstLastNumbers "2two2two")),
--       TestCase (assertEqual "for concatFirstLastNumbers 'this is a long string with 4 and nine in it'," 49 (concatFirstLastNumbers "this is a long string with 4 and nine in it")),
--       TestCase (assertEqual "for concatFirstLastNumbers 'onet2'," 12 (concatFirstLastNumbers "onet2")),
--       TestCase (assertEqual "for concatFirstLastNumbers 'tone2'," 12 (concatFirstLastNumbers "tone2")),
--       TestCase (assertEqual "for concatFirstLastNumbers 'tonep2heighto'," 18 (concatFirstLastNumbers "tonep2heighto")),
--       TestCase (assertEqual "for concatFirstLastSpelledNumbers 'twofold1three'," 23 (concatFirstLastNumbers "twofold1three")),
--       TestCase (assertEqual "for concatFirstLastSpelledNumbers ' ninexzhtqsr6hnftrbbnsevensevenoneightq'," 98 (concatFirstLastNumbers "ninexzhtqsr6hnftrbbnsevensevenoneightq"))
--     ]

-- main :: IO Counts
-- -- main = runTestTT $ TestList [testConcatFirstLastSpelledNumbers, testFindFirstNumber, testFindLastNumber]
-- main = runTestTT $ TestList [testFindLastNumber]

-- main = runTestTT $ TestList [testFindFirstNumber]

import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Test.HUnit (Test (TestCase, TestList), assertEqual, runTestTT)

makeTestCase :: (String, String) -> Test
makeTestCase (input, expectedOutput) =
  TestCase
    (assertEqual "Test concatFirstLastNumbers" (read expectedOutput :: Int) (concatFirstLastNumbers input))

loadLines :: FilePath -> IO [String]
loadLines path = do
  handle <- openFile path ReadMode
  contents <- hGetContents handle
  return (lines contents)

main :: IO ()
main = do
  inputLines <- loadLines "input.txt"
  expectedOutputLines <- loadLines "result.txt"
  let testCases = map makeTestCase (zip inputLines expectedOutputLines)
  runTestTT (TestList testCases)
  return ()
