module Main where

import Data.Bifunctor
import Data.Either (partitionEithers)
import Data.List (find, sortOn)
import Data.List.Split
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Tuple (swap)
import Data.Tuple.Extra (both)

main :: IO ()
main = do
  file <- readFile "d05/real.txt"
  let problem = parse file
  print $ uncurry d4 problem

d4 :: [(Int, Int)] -> [[Int]] -> (Int, Int)
d4 possibleRules = both sum . partitionEithers . map d4'
  where
    d4' line = if sortedList == line then Left sum else Right sum
      where
        rules = filter (uncurry (&&) . both (`S.member` S.fromList line)) possibleRules
        mkMap = M.fromListWith S.union . map (second S.singleton)
        backwardMap = mkMap $ map swap rules

        -- from whatever initial number we picked, crawl backwards until we're at the start
        walkBackwards x = maybe x (walkBackwards . S.findMin) (M.lookup x backwardMap)
        firstNumber = walkBackwards (head line)

        sortedRules = sortRules firstNumber (mkMap rules) (removeAllFromValues firstNumber backwardMap)
        ruleMap = M.fromList $ zip sortedRules [0 ..]
        sortedList = sortOn (ruleMap M.!) line
        sum = sortedList !! (length line `div` 2)

removeAllFromValues x = M.map (S.delete x)

-- given x is the start, then look for a rule x|y where there is no rule z|y
-- then remove y|* from the rules, then repeat
sortRules :: Int -> Map Int (Set Int) -> Map Int (Set Int) -> [Int]
sortRules x forwardMap backwardMap = x : maybe [] go maybeNext
  where
    go next = sortRules next forwardMap (removeAllFromValues next backwardMap)
    maybeNext = M.lookup x forwardMap >>= find (S.null . (M.!) backwardMap)

parse :: String -> ([(Int, Int)], [[Int]])
parse = parsePairs . span (/= "") . lines
  where
    parsePairs (rules, "" : rows) = (map (unsafePair . splitOn "|") rules, map (map read . splitOn ",") rows)
    unsafePair [a, b] = (read a, read b)
