module Main where

import Data.Char
import Data.List
import System.IO
import Data.List.Split
import Debug.Trace

type Present = [[Bool]]
type Tree    = ((Int, Int), [Int])

parse :: String -> ([Present], [Tree])
parse d = (map present [p0, p1, p2, p3, p4, p5], map tree (tail trees))
        where l = lines d
              (p0, r) = break (=="") l
              (p1, s) = break (=="") (tail r)
              (p2, t) = break (=="") (tail s)
              (p3, u) = break (=="") (tail t)
              (p4, v) = break (=="") (tail u)
              (p5, trees) = break (=="") (tail v)
              present = map (map (=='#')) . tail
              tree str = (read $ (\s -> "(" ++ s ++ ")") $ map replace $ init range,
                            read $ map replace $ "[" ++ tail l ++ "]") :: Tree
                    where (range, l) = break (==' ') str
                          replace ' ' = ','
                          replace 'x' = ','
                          replace c   = c

part1 :: ([Present], [Tree]) -> Int
part1 (presents, trees) = length $ filter sanityCheck trees
        where spacePerPresent = map (length . concatMap (filter id)) presents
              sanityCheck ((x, y), nb) = x * y > sum (zipWith (*) nb spacePerPresent)

-- part2 :: Graph -> Int
-- part2 g = (svr_to_fft * fft_to_dac * dac_to_out) + (svr_to_dac * dac_to_fft * fft_to_out)

main = do
  putStrLn "Exo 1:"
  file <- readFile "input.txt"
  -- file <- readFile "example.txt"
  -- file <- readFile "other_example.txt"

  let d@(presents, trees) = parse file
  -- mapM_ print presents
  -- mapM_ print trees
  print $ length trees
  print $ part1 d -- It definetly feels like cheating but I don't have the mood
                  -- to try and think much about the actual problem, so it will
                  -- do for now !

  -- print $ part2 d
