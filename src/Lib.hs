module Lib where

import Control.Applicative ((<|>))
import Data.Char (isAlpha, isDigit)
import Data.List (find, foldl, inits, isInfixOf, isPrefixOf, isSuffixOf)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)

findLastNumber :: String -> Maybe Int
findLastNumber str = listToMaybe . mapMaybe (checkForNumber . reverse) . inits . reverse $ str
  where
    checkForNumber xs
      | not (null xs) && isDigit (head xs) = Just (read [head xs])
      | otherwise = findWordNumber xs

findWordNumber :: String -> Maybe Int
findWordNumber xs = fmap snd . find (\(word, _) -> word `isPrefixOf` xs) $ spelledNumbers

findFirstNumber :: String -> Maybe Int
findFirstNumber str = go str Nothing
  where
    go [] acc = acc
    go s@(x : xs) acc
      | Just num <- acc = Just num
      | otherwise = go xs (acc <|> findSpelledNumber s <|> findDigit x)

    findSpelledNumber s = foldr (\(word, num) acc -> if word `isPrefixOf` s then Just num else acc) Nothing spelledNumbers

    findDigit c
      | '0' <= c && c <= '9' = Just $ read [c]
      | otherwise = Nothing

concatFirstLastNumbers :: String -> Int
concatFirstLastNumbers str =
  fromMaybe
    0
    ( do
        firstNum <- findFirstNumber str
        lastNum <- findLastNumber str
        return (read $ show firstNum ++ show lastNum)
    )

spelledNumbers :: [(String, Int)]
spelledNumbers =
  [ ("one", 1),
    ("two", 2),
    ("three", 3),
    ("four", 4),
    ("five", 5),
    ("six", 6),
    ("seven", 7),
    ("eight", 8),
    ("nine", 9)
  ]
