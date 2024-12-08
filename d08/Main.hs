module Main where

import Control.Monad (guard)
import Data.Bifunctor
import Data.Map (Map)
import Data.Map qualified as M
import Data.NumInstances.Tuple ()
import Data.Set qualified as S
import Data.Tuple.Extra (both, dupe)

main :: IO ()
main = do
  file <- readFile "d08/real.txt"
  let problem = parse file
  print $ d8a problem
  print $ d8b problem

parse :: String -> (Int, Map Char [(Int, Int)])
parse = bimap length (M.fromListWith (++) . coords) . dupe . lines
  where
    coords lines' = do
      (y, cs) <- zip [0 ..] lines'
      (x, c) <- zip [0 ..] cs
      guard $ c /= '.'
      return (c, [(x, y)])

-- look one diff forward from c0, one diff back from c1
d8a :: (Int, Map Char [(Int, Int)]) -> Int
d8a = d8 $ \(c0, c1) -> [c0 + (c0 - c1), c1 - (c0 - c1)]

-- look w diffs forward, w diffs back from c0
d8b :: (Int, Map Char [(Int, Int)]) -> Int
d8b (w, problem) = d8 nodes (w, problem)
  where
    nodes (c0, c1) = [c0 + both (* m) (c0 - c1) | m <- [-w .. w]]

d8 :: (((Int, Int), (Int, Int)) -> [(Int, Int)]) -> (Int, Map Char [(Int, Int)]) -> Int
d8 genNodes (w, chars) = S.size . S.fromList $ do
  charPlaces <- M.elems chars
  coordPairs' <- coordPairs charPlaces
  (uncurry (&&) . both (between w)) `filter` genNodes coordPairs'

between w x = x >= 0 && x < w
coordPairs l = [(x, y) | x <- l, y <- l, x < y]
