module Main where

import Data.Char
import Data.List
import System.IO
import Debug.Trace

parse :: String -> [(Int, Int)]
parse = map (\s -> (convert $ head s, read $ tail s)) . lines
            where convert 'L' = -1
                  convert 'R' = 1

part1 :: [(Int, Int)] -> Int
part1 = length . filter (==0) . foldl (\x (s, v) -> (head x + s * v) `mod` 100 : x) [50]

part2 :: [(Int, Int)] -> Int
part2 = helper 50
        where helper curr [] = 0
              helper curr ((dir, amount) : tl) = qot + turn + helper rem tl
                    where qot = amount `div` 100
                          rest = amount - 100 * qot
                          next = curr + dir * rest
                          turn = abs $ next `div` 100
                          rem  = next `mod` 100

main = do
  putStrLn "Exo 1:"
  file <- readFile "input.txt"
  -- file <- readFile "example.txt"

  let d = parse file
  print $ part1 d

  putStrLn "Exo 2:"
  print $ part2 d
