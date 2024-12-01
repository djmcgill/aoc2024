module Main where

import Data.List (sort)
import Data.Map (fromListWith, findWithDefault)
import Data.Tuple.Extra (dupe, both, (***), (&&&))
main :: IO ()
main = do 
    file <- readFile "real.txt"
    let problem = parse file 
    print $ d1a problem
    print $ d1b problem

parse :: String -> [(Int, Int)]
parse = map ((\[a,b] -> (a,b)) . map read . words) . lines

d1a :: [(Int, Int)] -> Int
d1a = sum . uncurry (zipWith ((abs.) . (-))) . both sort . unzip

d1b :: [(Int, Int)] -> Int
d1b st = sum $ map (\x -> x * findWithDefault 0 x secondCounts) fsts
    where
    (fsts, snds) = unzip st
    secondCounts = fromListWith (+) $ map (,1) snds
