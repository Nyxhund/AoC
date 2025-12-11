module Main where

import Data.Char
import Data.List
import System.IO
import Data.List.Split
import Debug.Trace
import Data.Function.Memoize
import qualified Data.Map as M

type Graph = M.Map String [String]

parse :: String -> Graph
parse = M.fromList . map ((\x -> (init $ head x, tail x)). words) . lines

countPaths :: Graph -> String -> String -> Int
countPaths g target = memoFix go
        where go f str | str == target  = 1
                       | otherwise      = sum $ map f (M.findWithDefault [] str g)

part1 :: Graph -> Int
part1 g = countPaths g "out" "you"

part2 :: Graph -> Int
part2 g = (svr_to_fft * fft_to_dac * dac_to_out) + (svr_to_dac * dac_to_fft * fft_to_out)
        where svr_to_fft = countPaths g "fft" "svr"
              svr_to_dac = countPaths g "dac" "svr"
              dac_to_fft = countPaths g "fft" "dac"
              fft_to_dac = countPaths g "dac" "fft"
              fft_to_out = countPaths g "out" "fft"
              dac_to_out = countPaths g "out" "dac"

main = do
  putStrLn "Exo 1:"
  file <- readFile "input.txt"
  -- file <- readFile "example.txt"
  -- file <- readFile "other_example.txt"

  let d = parse file
  -- print d
  print $ part1 d

  print $ part2 d
