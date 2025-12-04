module Main where

import Data.Char
import Data.List
import System.IO
import Debug.Trace
import qualified Data.Map as Map

parse :: String -> Map.Map (Int, Int) Int
parse = Map.fromList . concatMap f . zip [0..] . lines
        where m '.' = 0
              m '@' = 1
              f (i, str) = map (\(x, y) -> ((i, x), y)) $ zip [0..] $ map m str

part1 :: Map.Map (Int, Int) Int -> Int
part1 l = Map.foldlWithKey m 0 l
        where r = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
              m accu (x, y) b | b == 1 && 4 > (sum $ map (\(i, j) -> Map.findWithDefault 0 (x + i, y + j) l) r) = 1 + accu
                              | otherwise = accu

auxPart2 :: Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int
auxPart2 l = Map.mapWithKey m l
        where r = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
              m (x, y) b | b == 1 && 4 > (sum $ map (\(i, j) -> Map.findWithDefault 0 (x + i, y + j) l) r) = 0
                         | otherwise = b

part2 :: Map.Map (Int, Int) Int -> Int
part2 l = sum $ takeWhile (/=0) $ zipWith (-) ite (tail ite)
        where ite = map (Map.foldl (+) 0) $ iterate auxPart2 l

main = do
  putStrLn "Exo 1:"
  file <- readFile "input.txt"
  -- file <- readFile "example.txt"

  let d = parse file
  -- print d
  print $ part1 d

  putStrLn "Exo 2:"
  print $ part2 d


-- (0,9),(0,8),(0,6),(0,5),(0,4),(0,3),(0,2),(0,0)]
--
-- ..xx.xx@x.  (0,2)  (0,3) (0,5) (0,6) (0,8)
-- x@@.@.@.@@  (1,0)
-- @@@@@.x.@@  (2,6)
-- @.@@@@..@.
-- x@.@@@@.@x  (4,0)  (4,9)
-- .@@@@@@@.@
-- .@.@.@.@@@
-- x.@@@.@@@@  (6.0)
-- .@@@@@@@@.
-- x.x.@@@.x.


