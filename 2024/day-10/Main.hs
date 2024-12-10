module Main where

import Data.List
import System.IO
import Data.Char
import Data.Matrix (fromLists, getElem, Matrix, nrows, ncols)
import Debug.Trace

parse :: String -> ([(Int, Int)], Matrix Int)
parse input = (concatMap (\(i, l) -> map (\x -> (i, x)) l) $ zip [1..] $ map (map (1 +) . elemIndices 0) mat, fromLists mat)
    where mat = map (map digitToIntDefault) $ lines input
          digitToIntDefault c | c == '.'  = -1
                              | otherwise = digitToInt c

walk :: Int -> Matrix Int -> (Int, Int) -> [(Int, Int)]
walk target mat (x, y)
    | x == 0                    = [] --trace (show (x, y) ++ " Ended X  0") []
    | x - 1 == ncols mat        = [] --trace (show (x, y) ++ " Ended X -1") []
    | y == 0                    = [] --trace (show (x, y) ++ " Ended Y  0") []
    | y - 1 == nrows mat        = [] --trace (show (x, y) ++ " Ended Y -1") []
    | getElem x y mat /= target = [] --trace (show (x, y) ++ " Ended Targ") []
    | target == 9               = [(x, y)] --trace "Found" [(x, y)]
    | otherwise                 = rmdups $
                                  walk (target + 1) mat (x + 1, y) ++
                                  walk (target + 1) mat (x - 1, y) ++
                                  walk (target + 1) mat (x, y + 1) ++
                                  walk (target + 1) mat (x, y - 1)
        where rmdups = map head . group . sort

walkWithoutRemoving :: Int -> Matrix Int -> (Int, Int) -> [(Int, Int)]
walkWithoutRemoving target mat (x, y)
    | x == 0                    = [] --trace (show (x, y) ++ " Ended X  0") []
    | x - 1 == ncols mat        = [] --trace (show (x, y) ++ " Ended X -1") []
    | y == 0                    = [] --trace (show (x, y) ++ " Ended Y  0") []
    | y - 1 == nrows mat        = [] --trace (show (x, y) ++ " Ended Y -1") []
    | getElem x y mat /= target = [] --trace (show (x, y) ++ " Ended Targ") []
    | target == 9               = [(x, y)] --trace "Found" [(x, y)]
    | otherwise                 =
                                  walkWithoutRemoving (target + 1) mat (x + 1, y) ++
                                  walkWithoutRemoving (target + 1) mat (x - 1, y) ++
                                  walkWithoutRemoving (target + 1) mat (x, y + 1) ++
                                  walkWithoutRemoving (target + 1) mat (x, y - 1)

part1 :: ([(Int, Int)], Matrix Int) -> Int
part1 (coords, mat) = (sum . map (length . walk 0 mat)) coords

part2 :: ([(Int, Int)], Matrix Int) -> Int
part2 (coords, mat) = (sum . map (length . walkWithoutRemoving 0 mat)) coords

main = do
  file <- readFile "input.txt"
  -- file <- readFile "example.txt"

  putStrLn "Exo 1:"
  let input@(startCoords, mat) = parse file
  print startCoords
  print "-----"
  print $ part1 input
  -- traverse print $ map (walk 0 mat) startCoords

  putStrLn "\nExo 2:"
  print $ part2 input
