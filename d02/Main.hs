module Main where

main :: IO ()
main = do
  file <- readFile "d02/real.txt"
  let problem = parse file
  print $ d2a problem
  print $ d2b problem

parse :: String -> [[Int]]
parse = map (map read . words) . lines

d2a :: [[Int]] -> Int
d2a = length . filter checkRow . map diffs
  where
    diffs l = zipWith (-) l (tail l)
    checkRow x = all (checkBound 1 3) x || all (checkBound (-3) (-1)) x
    checkBound l u x = x >= l && x <= u

-- fine just brute force it
d2b :: [[Int]] -> Int
d2b = length . filter checkRow
  where
    diffs l = zipWith (-) l (tail l)
    checkHalf u l = all (checkBound u l) . diffs
    checkPossibility l = checkHalf 1 3 l || checkHalf (-3) (-1) l
    checkRow = any checkPossibility . possibleLists
    checkBound l u x = x >= l && x <= u
    -- for each element, remove it
    -- [1,2,3,4] -> [[2,3,4],[1,3,4],[1,2,4],[1,2,3]]
    possibleLists (x : xs) = xs : map (x :) (possibleLists xs)
    possibleLists [] = []

-- d2b' :: [[Int]] -> [Bool]
-- d2b' = map checkRow
--   where
--     checkRow l = it 1 3 False l || it (-3) (-1) False l
--     cond l u a b = trace (show (a, b)) $ (a - b) >= l && (a - b) <= u
--     it l u existingProblem (a : b : c : xs) =
--       if cond l u a b
--         -- if it's true then we're guchi
--         then it l u existingProblem (b : c : xs)
--         else not existingProblem && (it l u True (a : c : xs) ||
--                                      it l u True (b : c : xs) FIXME: also need to check `cond (ix -1) b` in this case)
--     it l u True [a, b] = cond l u a b
--     it _ _ False [_, _] = True
--     -- we removed the last level. probably shouldn't happen.
--     it _ _ _ _ = undefined
