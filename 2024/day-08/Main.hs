module Main where

import Data.List
import System.IO
import Debug.Trace

getInputs :: Int -> Int -> [String] -> String -> [[(Int, Int)]] -> [[(Int, Int)]]
getInputs _ _ [] _ indexes = indexes
getInputs x y ([] : curr) freq indexes = getInputs 0 (y + 1) curr freq indexes
getInputs x y ((c:tc) : curr) freq indexes = getInputs (x + 1) y (tc : curr) freq
            (if c /= '.' then
                maybe indexes (\i -> take i indexes ++ [(x, y) : (indexes !! i)] ++ drop (i+1) indexes) index
            else
                indexes)
                    where index = elemIndex c freq

parse :: String -> [[(Int, Int)]]
parse input = getInputs 0 0 (lines input) frequencies $ replicate (length frequencies) []
        where listFreq = foldr (\c accu -> case () of
                            _ | c /= '.' && c /= '\n' && notElem c accu -> c : accu
                              | otherwise                  -> accu) ""
              frequencies = listFreq input

process :: [(Int, Int)] -> [(Int, Int)]
process [_]    = []
process ((x, y) : ta) = concatMap genAntinodes ta ++ process ta
            where genAntinodes (xP, yP) =
                        let xDiff = xP - x in
                        let yDiff = yP - y in
                        [(x - xDiff, y - yDiff), (xP + xDiff, yP + yDiff)]

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

part1 :: Int -> Int -> [[(Int, Int)]] -> Int
part1 width height = length . filter (\(x, y) -> x >= 0 && x < width && y >= 0 && y < height) . rmdups . concatMap process

processExtended :: [(Int, Int)] -> [(Int, Int)]
processExtended [_]    = []
processExtended ((x, y) : ta) = concatMap genAntinodes ta ++ processExtended ta
            where coeffsBefore = take 100 [-1..]
                  coeffsAfter  = take 100 [1..]
                  genAntinodes (xP, yP) =
                        let xDiff = xP - x in
                        let yDiff = yP - y in
                        map (\i -> (x - i * xDiff, y - i * yDiff)) coeffsBefore ++ map (\i -> (xP + i * xDiff, yP + i * yDiff)) coeffsAfter

part2 :: Int -> Int -> [[(Int, Int)]] -> Int
part2 width height = length . filter (\(x, y) -> x >= 0 && x < width && y >= 0 && y < height) . rmdups . concatMap processExtended

main = do
  file <- readFile "input.txt"
  -- file <- readFile "example.txt"
  let m = lines file
  let height = length m
  let width = length $ head m

  let input = parse file
  -- print $ input

  putStrLn "Exo 1:"
  print $ part1 width height input

  -- 1144 too low
  putStrLn "\nExo 2:"
  print $ part2 width height input
