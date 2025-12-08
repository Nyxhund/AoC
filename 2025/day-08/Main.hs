module Main where

import Data.Char
import Data.List
import System.IO
import Data.List.Split
import Debug.Trace
import GHC.Float
import Data.Ord

type Point = (Int, Int, Int)

parse :: String -> [Point]
parse = map ((\(x : y : [z]) -> (x, y, z)) . map read . splitOn ",") . lines

euclidianDist :: Point -> Point -> Float
euclidianDist (x, y, z) (x', y', z') = sqrt (xP * xP + yP * yP + zP * zP)
        where xP = int2Float (x - x')
              yP = int2Float (y - y')
              zP = int2Float (z - z')

combine accu (_, a, b) | la == lb  = accu
                       | otherwise = (la ++ lb) : delete la (delete lb accu)
      where la = maybe [] id (find (elem a) accu)
            lb = maybe [] id (find (elem b) accu)

pairs []       = []
pairs (a : ta) = zip (repeat a) ta ++ pairs ta

part1 :: Int -> [Point] -> Int
part1 connections l = product $ take 3 $ sortBy (comparing Data.Ord.Down) $ map length $ foldl combine (map (: []) l) toProcess
        where p = pairs l
              toProcess = take connections $ sortBy (\(x, _, _) (y, _, _) -> compare x y) $ map (\(a, b) -> (euclidianDist a b, a, b)) p

part2 :: [Point] -> Int
part2 l = (\(_, (x, _, _), (x', _, _)) -> x * x') $ rep (map (: []) l) (zip (tail toProcess) toProcess )
        where p = pairs l
              toProcess = sortBy (\(x, _, _) (y, _, _) -> compare x y) $ map (\(a, b) -> (euclidianDist a b, a, b)) p
              rep _    [(_, answer)] = answer
              rep accu ((toP, answer) : ta) | length accu == 1 = answer
                                            | otherwise        = rep (combine accu toP) ta

main = do
  putStrLn "Exo 1:"
  file <- readFile "input.txt"
  -- file <- readFile "example.txt"

  let d = parse file
  -- print d
  print $ part1 1000 d
  -- print $ part1 10 d  -- FOR EXAMPLE

  putStrLn "Exo 2:"
  print $ part2 d
