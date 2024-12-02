module Main where

import Data.List
import System.IO

parse :: String -> [[Int]]
parse = map (map read . words) . lines

substract :: [Int] -> [Int]
substract [] = []
substract [a] = []
substract (a : b) = a - head b : substract b

countElem e = length . filter (e ==)

part1 :: [[Int]] -> Int
part1 = length . filter (all (\x -> x <= 3 && x >= 1)) . map (map abs) . filter (\x -> all (>=0) x || all (<=0) x) . map substract

part1Individual :: [Int] -> Int
part1Individual a = length $
                  filter (all (\x -> x <= 3 && x >= 1)) $
                  map (map abs) $
                  filter (\x -> all (>=0) x || all (<=0) x)
                  [substract a]

part2Individual :: [Int] -> [Int] -> Int
part2Individual _ [] = 0
part2Individual before (a : ta) = if part1Individual (before ++ ta) == 1 then
                                      1
                                  else
                                      part2Individual (before ++ [a]) ta

part2 :: [[Int]] -> Int
part2 = sum . map (part2Individual [])

main = do
  file <- readFile "input.txt"
  -- file <- readFile "example.txt"
  let input = parse file
  -- print input

  putStrLn "Exo 1:"
  print $ part1 input
  putStrLn "\nExo 2:"
  print $ part2 input
