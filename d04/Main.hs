module Main where

import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
  file <- readFile "d04/real.txt"
  let problem = parse file
  print $ d4a problem
  print $ d4b problem

d4a :: (Int, Map (Int, Int) Char) -> Int
d4a (w, problem) = countFinds hori + countFinds verti + countFinds downRightDiags + countFinds upLeftDiags
  where
    countFinds ixs = sum . map find . map (map ((Map.!) problem)) $ ixs w
    hori w = [[(x, y) | y <- [0 .. w - 1]] | x <- [0 .. w - 1]]
    verti w = [[(x, y) | x <- [0 .. w - 1]] | y <- [0 .. w - 1]]
    upLeftDiags w =
      [[(total - y, y) | y <- [0 .. total]] | total <- [0 .. w - 1]]
        ++ [[(w - y - 1, w - total + y - 1) | y <- [0 .. total]] | total <- reverse [0 .. w - 2]]
    downRightDiags w = map (map (\(x, y) -> (x, w - y - 1))) (upLeftDiags w)

    find :: String -> Int
    find ('X' : 'M' : 'A' : 'S' : xs) = 1 + find ('M' : 'A' : 'S' : xs)
    find ('S' : 'A' : 'M' : 'X' : xs) = 1 + find ('A' : 'M' : 'X' : xs)
    find (_ : xs) = find xs
    find [] = 0

d4b :: (Int, Map (Int, Int) Char) -> Int
d4b (w, problem) = sum $ map checkCross ixs
  where
    checkCross ix = if diags == 2 then 1 else 0
      where
        diags = length (filter (\c -> c == "MAS" || c == "SAM") crosses)
        crossesIxs = cross ix
        crosses = map (map ((Map.!) problem)) crossesIxs
    cross (x, y) = [[(x - 1, y - 1), (x, y), (x + 1, y + 1)], [(x + 1, y - 1), (x, y), (x - 1, y + 1)]]
    ixs = [(x, y) | x <- [1 .. w - 2], y <- [1 .. w - 2]]

parse :: String -> (Int, Map (Int, Int) Char)
parse s = (w, chars)
  where
    lines' = lines s
    w = length lines'
    chars = Map.fromList . concatMap toIx . zip [0 ..] . map (zip [0 ..]) $ lines'
    toIx :: (Int, [(Int, Char)]) -> [((Int, Int), Char)]
    toIx (y, xcs) = map (\(x, c) -> ((x, y), c)) xcs
