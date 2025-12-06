module Main where

import Data.Char
import Data.List
import System.IO
import Data.List.Split
import Debug.Trace

parse :: String -> ([[Int]], [String])
parse str = (numbers, opes)
        where d = (map words . lines) str
              l = length d
              numbers = (map (map read) . take (l-1)) d
              opes = (head . drop (l-1)) d

part1 :: ([[Int]], [String]) -> Int
part1 (numbers, opes) = sum $ zipWith combine opes numbers'
        where numbers' = transpose numbers
              combine "+" = sum
              combine "*" = product

part2 :: String -> Int
part2 str = sum $ zipWith combine opesAdapted numbersAdapted
        where d = lines str
              l = length d
              numbers = take (l-1) d
              opes = (head . drop (l-1)) d
              slice [] = []
              slice str = (head str : (takeWhile (==' ') $ tail str)) : slice (dropWhile (==' ') $ drop 1 str)
              opes' = slice opes
              numbers' = map (help (map length opes')) numbers
                    where help _ []      = []
                          help index str = f : help (tail index) ft
                                where (f, ft) = splitAt (head index) str
              numbersAdapted = map (map (read . head) . filter (/=[]) . map words . transpose) $ transpose numbers'
              opesAdapted = map (head . words) opes'
              combine "+" = sum
              combine "*" = product

main = do
  putStrLn "Exo 1:"
  file <- readFile "input.txt"
  -- file <- readFile "example.txt"

  let d = parse file
  -- print d
  print $ part1 d

  putStrLn "Exo 2:"
  print $ part2 file
