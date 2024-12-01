module Main where


import Data.Char
import Data.List
import System.IO

countElem e = length . filter (e ==)

minOfDiff :: [Int] -> [Int] -> Int
minOfDiff a b = sum $ map abs $ zipWith (flip (-)) sortedA sortedB
    where sortedA = sort a
          sortedB = sort b

similarityScore :: [Int] -> [Int] -> Int
similarityScore a b = sum $ map (\x -> x * countElem x b) a

main = do
  putStrLn "Exo 1:"
  file <- readFile "input.txt"
  -- file <- readFile "example.txt"
  let lists = transpose $ map words $ lines file
  let list1 = map read $ head lists
  let list2 = map read $ head $ tail lists

  print $ minOfDiff list1 list2

  putStrLn "Exo 2:"
  print $ similarityScore list1 list2
