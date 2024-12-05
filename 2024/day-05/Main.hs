module Main where

import Data.List
import System.IO
import Data.List.Split
import Debug.Trace
import Data.Ord

parse :: String -> ([(Int, Int)], [[Int]])
parse input = (toTuple $ splitAndRead "|" instructions, splitAndRead "," updates)
        where allLines = lines input
              instructions = takeWhile (/="") allLines
              updates = drop 1 $ dropWhile (/="") allLines
              splitAndRead c = map (map read . splitOn c)
              toTuple = map (\x -> (head x, head $ tail x))

getMiddle :: [a] -> a
getMiddle l = head $ drop (length l `div` 2) l

checkUpdate :: [(Int, Int)] -> [Int] -> Bool
checkUpdate _ []           = False
checkUpdate pairs (e : te) = any (`elem` pairs) (zip te (repeat e)) || checkUpdate pairs te

part1 :: ([(Int, Int)], [[Int]]) -> Int
part1 (pairs, updates) = sum $ map getMiddle $ filter (not . checkUpdate pairs) updates

sorter :: [(Int, Int)] -> Int -> Int -> Ordering
sorter pairs a b = if (a, b) `elem` pairs then
                        GT
                   else
                        LT

part2 :: ([(Int, Int)], [[Int]]) -> Int
part2 (pairs, updates) = sum $ map (getMiddle . sortBy (sorter pairs)) $ filter (checkUpdate pairs) updates

main = do
  file <- readFile "input.txt"
  -- file <- readFile "example.txt"

  let input = parse file
  putStrLn "Exo 1:"
  print $ part1 input

  putStrLn "\nExo 2:"
  print $ part2 input
