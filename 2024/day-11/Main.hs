module Main where

import Data.List
import System.IO
import Data.List.Split

parse :: String -> [Int]
parse = map read . splitOn " " . head . lines

blink :: [Int] -> [Int]
blink = concatMap rules
        where rules nb | nb == 0  = [1]
                       | even (length $ show nb)
                            = let (f, s) = splitAt (div (length $ show nb) 2) $ show nb in
                               map read [ f, s ]
                       | otherwise = [2024 * nb]

part1 :: [Int] -> Int
part1 = length . (!! 25) . iterate blink

part2 :: [Int] -> Int
part2 = length . (!! 75) . iterate blink

main = do
  file <- readFile "input.txt"
  -- file <- readFile "example.txt"
  let input = parse file

  print input

  putStrLn "Exo 1:"
  print $ part1 input

  putStrLn "\nExo 2:"
  print $ part2 input
