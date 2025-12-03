module Main where

import Data.Char
import Data.List
import System.IO
import Data.List.Split
import GHC.Float
import Debug.Trace

parse :: String -> [(Int, Int)]
parse = map ((\x -> (head x, head $ tail x)) . map read . splitOn "-") . splitOn ","

digits :: Int -> Int
digits number = floor ( logBase 10 (int2Float number)) + 1

cleanUp :: [(Int, Int)] -> [(Int, Int)]
cleanUp = map (\(x, y) -> (if even (digits x) then x else 10 ^ digits x,
                        if even (digits y) then y else 10 ^ (digits y - 1) - 1))

findDoubles :: (Int, Int) -> [Int]
findDoubles (x, y) = filter (\i -> i >= x && i <= y) $ map (\n -> read $ show n ++ show n) l
            where xString = show x
                  size = div (length xString) 2
                  start :: Int = read $ take size xString
                  end   :: Int = read $ take size $ show y
                  l = [start .. end]

part1 :: [(Int, Int)] -> Int
part1 = sum . concatMap findDoubles . cleanUp

check :: Int -> Bool
check s = any ((\x -> length x > 1 && all (== head x) x) . (`chunksOf` str)) [1 .. l `div` 2]
        where str = show s
              l = length str

findRepeating :: (Int, Int) -> [Int]
findRepeating (x, y) = filter check [ x .. y ]

part2 :: [(Int, Int)] -> Int
part2 = sum . concatMap findRepeating

main = do
  putStrLn "Exo 1:"
  file <- readFile "input.txt"
  -- file <- readFile "example.txt"

  let d = parse file
  -- print d
  -- print $ cleanUp d
  -- let cleaned = cleanUp d
  -- let pair = head $ tail cleaned
  -- print pair

  print $ part1 d
  putStrLn "Exo 2:"

  print $ part2 d
