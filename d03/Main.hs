module Main where

import Data.List (foldl')
import Text.Regex.TDFA

main :: IO ()
main = do
  problem <- readFile "d03/real.txt"
  print $ d3a problem
  print $ d3b problem

d3a :: String -> Int
d3a = sum . map (processSubmatches . getSubmatches) . getMatches
  where
    mulRegex = "mul\\(([[:digit:]]+),([[:digit:]]+)\\)"
    getMatches :: String -> [String] = getAllTextMatches . (=~ mulRegex)
    getSubmatches :: String -> [String] = getAllTextSubmatches . (=~ mulRegex)
    processSubmatches = \[_, a, b] -> read a * read b

d3b :: String -> Int
d3b = fst . processMatches . getMatches
  where
    mulRegex = "mul\\(([[:digit:]]+),([[:digit:]]+)\\)|do\\(\\)|don't\\(\\)"
    getMatches :: String -> [String] = getAllTextMatches . (=~ mulRegex)
    processMatches = foldl' f (0, True)
    f (acc, _) "don't()" = (acc, False)
    f (acc, _) "do()" = (acc, True)
    f (acc, False) _ = (acc, False)
    f (acc, True) sub = (acc + read a * read b, True)
      where
        [_, a, b] = getAllTextSubmatches (sub =~ mulRegex)
