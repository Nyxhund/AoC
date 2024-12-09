module Main where

import Data.List
import System.IO
import Data.List.Split
import Debug.Trace

parse :: String -> [(Int, [Int])]
parse text = map (\x -> (read $ init $ head x, map read $ tail x)) equations
            where equations = (map (splitOn " ") . lines) text

checkEquation :: Int -> (Int, [Int]) -> Bool
checkEquation curr (res, [])     = res ==  curr
checkEquation curr (res, a : ta) = checkEquation (curr + a) (res, ta) || checkEquation (curr * a) (res, ta)

checkEquation2 :: Int -> (Int, [Int]) -> Bool
checkEquation2 curr (res, [])     = res ==  curr
checkEquation2 curr (res, a : ta) = checkEquation2 (curr + a) (res, ta) ||
                                    checkEquation2 (curr * a) (res, ta) ||
                                    checkEquation2 (read $ show curr ++ show a) (res, ta)

part1 :: [(Int, [Int])] -> Int
part1 = sum . map fst . filter (\(res, a : ta) -> checkEquation a (res, ta))

part2 :: [(Int, [Int])] -> Int
part2 = sum . map fst . filter (\(res, a : ta) -> checkEquation2 a (res, ta))

main = do
  file <- readFile "input.txt"
  -- file <- readFile "example.txt"
  let input = parse file

  putStrLn "Exo 1:"
  print $ part1 input

  putStrLn "\nExo 2:"
  print $ part2 input
