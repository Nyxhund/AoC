module Main where

import Data.Char
import Data.List
import System.IO
import Data.List.Split
import Debug.Trace
import qualified Data.IntMap as IntMap

parse :: String -> [(Int, Int)]   -- Thanks Sheinxy for the trick
parse = map (\s -> read $ "(" ++ s ++ ")") . lines

part1 :: [(Int, Int)] -> Int
part1 points = maximum $ map (\((x, y), (x', y')) -> (abs (x - x') + 1) * (abs (y - y') + 1))
        [(b1, b2) | b1 <- points, b2 <- points, b1 < b2]
        -- Thanks Sheinxy again

genList points = nub $ [(b1, b2) | b1 <- points, b2 <- points, b1 < b2]

inRange :: Int -> (Int, Int) -> Bool
inRange a (x, y) = x <= a && a <= y

doesNotIntersects :: Int -> IntMap.IntMap (Int, Int) -> (Int, Int) -> Bool
doesNotIntersects x m (l, h) = 1 > length (filter id $ map (inRange x . (flip $ IntMap.findWithDefault (0, 0)) m) [min l h .. max l h])

part2 :: [(Int, Int)] -> Int -- [((Int, Int), (Int, Int))]
part2 l = maximum $ map (\((x, y), (x', y')) -> (abs (x - x') + 1) * (abs (y - y') + 1)) $
            filter (\((x, y), (x', y')) ->
                    doesNotIntersects y walls (x + 1, x' - 1) &&
                    doesNotIntersects y' walls (x + 1, x' - 1) &&
                    doesNotIntersects x lines (y + 1, y' - 1) &&
                    doesNotIntersects x' lines (y + 1, y' - 1))
                [(b1, b2) | b1 <- l, b2 <- l, b1 < b2]
                where toProcess = zip l (tail l ++ [head l])
                      f (left, right) ((a, b), (c, d)) | a == c    = (left, IntMap.insert a (min b d, max b d) right)
                                                       | otherwise = (IntMap.insert b (min a c, max a c) left, right)
                      (lines, walls) = foldl f (IntMap.empty, IntMap.empty) toProcess

-- (fromList [(1,(7,11)),(3,(2,7)),(5,(2,9)),(7,(9,11))],
-- fromList [(2,(3,5)),(7,(1,3)),(9,(5,7)),(11,(1,7))])


main = do
  putStrLn "Exo 1:"
  file <- readFile "input.txt"
  -- file <- readFile "example.txt"
  -- file <- readFile "other_example.txt"

  let d = parse file
  -- print d
  print $ part1 d

  putStrLn "Exo 2:" -- 94501256 too low
                    -- 115599078 too low
                    -- 1170070524 too low ;-;

  -- let (lines, walls) = part2 d
  -- print (lines, walls)
  -- let ((x, y), (x', y')) = ((2,5),(7,1))
  --
  -- print $ doesNotIntersects y walls (x + 1, x' - 1)
  -- print $ doesNotIntersects y' walls (x + 1, x' - 1)
  -- print $ doesNotIntersects x lines (y + 1, y' - 1)
  -- print $ doesNotIntersects x' lines (y + 1, y' - 1)
  --
  -- mapM_ print $ sortOn snd $ map (\((x, y), (x', y')) -> ( ((x, y), (x', y')), (abs (x - x') + 1) * (abs (y - y') + 1))) $ part2 d
  print $ part2 d
