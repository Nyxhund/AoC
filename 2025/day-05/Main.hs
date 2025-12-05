module Main where

import Data.Char
import Data.List
import System.IO
import Data.List.Split
import qualified Data.IntMap as IntMap
import Data.Maybe
import Debug.Trace

parse :: String -> ([(Int, Int)], [Int])
parse str = (ranges, ids)
        where l = lines str
              ranges = map ((\x -> (head x, head $ tail x)) . map read . splitOn "-") $ takeWhile (/= "") l
              ids = map read $ tail $ dropWhile (/= "") l

part1 :: ([(Int, Int)], [Int]) -> Int
part1 (ranges, ids) = length $ filter (\i -> any (\(x, y) -> x <= i && i <= y) ranges) ids

helper :: Maybe (Int, Int) -> Maybe (Int, Int) -> IntMap.IntMap Int -> (Int, Int) -> IntMap.IntMap Int
helper Nothing Nothing m (x, y) = IntMap.insert x y m
helper Nothing (Just (c, d)) m (x, y)
    | c > y     = IntMap.insert x y m
    | otherwise = IntMap.insert x d $ IntMap.delete c m

helper (Just (a, b)) Nothing m (x, y)
    | b < x     = IntMap.insert x y m
    | otherwise = IntMap.insert (min x a) (max y b) $ IntMap.delete a m

helper (Just (a, b)) (Just (c, d)) m (x, y)
    | x <= b && y >= c = IntMap.insert (min x a) (max y d) $ IntMap.delete c $ IntMap.delete a m
    | x <= b && y < c  = IntMap.insert (min x a) (max y b) $ IntMap.delete a m
    | x > b && y >= c  = IntMap.insert (min x c) (max y d) $ IntMap.delete c m
    | x > b && y < c   = IntMap.insert x y m

part2 :: ([(Int, Int)], [Int]) -> Int
part2 (ranges, _) = IntMap.foldlWithKey (\y x accu -> accu + y - x + 1) 0 $ merge ranges
        where merge = foldl update IntMap.empty
                where update m (x, y) = helper le gt m (x, y)
                        where le = IntMap.lookupLE x m
                              gt = IntMap.lookupGT x m

main = do
  putStrLn "Exo 1:"
  file <- readFile "input.txt"
  -- file <- readFile "example.txt"

  let d = parse file
  -- print d
  print $ part1 d

  putStrLn "Exo 2:"
  print $ part2 d
