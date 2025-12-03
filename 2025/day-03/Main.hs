module Main where

import Data.Char
import Data.List
import System.IO
import Data.List.Split
import Data.Bifunctor
import Debug.Trace

parse :: String -> [[Int]]
parse = map (map digitToInt) . lines

getMaxIndex :: Int -> [Int] -> (Int, Int)
getMaxIndex toFind = m (-1) (-1) 0
        where m val i _ [] = (val, i)
              m 9 i _ _ = (9, i)
              m val i index (t:tl) | toFind > length tl  + 1 = (val, i)
                                   | val >= t   = m val i (index + 1) tl
                                   | otherwise  = m t index (index + 1) tl

getMaxVoltage :: [Int] -> Int
getMaxVoltage l | index + 1 == length l = 10 * fst (getMaxIndex 1 $ take index l) + biggest
                | otherwise             = biggest * 10 + fst (getMaxIndex 1 $ drop (index + 1) l)
            where (biggest, index) = getMaxIndex 2 l

part1 :: [[Int]] -> Int
part1 = sum . map getMaxVoltage

getBestChoice :: Int -> Int -> [Int] -> Int
getBestChoice accu 0 _ = accu
getBestChoice accu nb l = getBestChoice (accu * 10 + value) (nb - 1) (drop (index + 1) l)
                where (value, index) = getMaxIndex nb l

part2 :: [[Int]] -> Int
part2 = sum . map (getBestChoice 0 12)

main = do
  putStrLn "Exo 1:"
  file <- readFile "input.txt"
  -- file <- readFile "example.txt"
  let d = parse file
  print $ part1 d

  putStrLn "Exo 2:"
  print $ part2 d

