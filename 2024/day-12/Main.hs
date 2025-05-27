module Main where

import Data.List
import System.IO
import Data.Matrix (fromLists, getElem, Matrix, nrows, ncols)
import qualified Data.Map.Strict as Map
import Debug.Trace

parse :: String -> Matrix Char
parse = fromLists . lines

wrappedGet :: Matrix Char -> Char -> (Int, Int) -> Int
wrappedGet mat target (a, b)
            | a < 1 || a > nrows mat || b < 1 || b > ncols mat   = 1
            | getElem a b mat /= target                          = 1
            | otherwise                                          = 0

process :: Char -> (Int, Matrix Char) -> (Int, Int) -> (Int, Matrix Char)
process c (curr, mat) (x, y) = (curr + nb, mat)
        where possible = [(x + 1, y), (x - 1, y), (x, y - 1), (x, y + 1)] :: [(Int, Int)]
              localWrap = wrappedGet mat c :: (Int, Int) -> Int
              nb = sum (map localWrap possible) :: Int

flood :: Char -> Matrix Char -> [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
flood c mat l (a, b)
            | (a, b) `elem` l                                  = l
            | a < 1 || a > nrows mat || b < 1 || b > ncols mat = l
            | getElem a b mat /= c                             = l
            | otherwise = foldl (flood c mat) l [(a + 1, b), (a - 1, b), (a, b + 1), (a, b - 1)]
        where appended = (a, b) : l

regions :: [(Int, Int)] -> Matrix Char -> [(Char, [(Int, Int)])]
regions [] _ = []
regions (a@(x, y) : ta) mat = if a `elem` visited then computed else (getElem x y mat, flood (getElem x y mat) mat [] a) : computed
        where computed = regions ta mat
              visited = concatMap snd computed

part1 :: String -> Int
part1 input = sum $ zipWith (*) (trace (show area) area) (trace (show perimeter) perimeter)
        where   keysGroup = group $ sort input
                accessMatrix = parse input
                width = nrows accessMatrix
                height = ncols accessMatrix
                indexes = concatMap (\(i, l) -> zip (repeat i) l) $ zip (take height [1..]) (replicate height $ take width [1..])
                keys = map head keysGroup :: String
                reg = regions indexes
                perimeter = map (fst . foldl (process) (0, accessMatrix)) reg
                area = map length keysGroup

main = do
  -- file <- readFile "input.txt"
  -- file <- readFile "example.txt"
  -- file <- readFile "smallerExample.txt"
  file <- readFile "secondExample.txt"

  putStrLn "Exo 1:"
  print $ part1 $ init file
  -- let indexes = zip (take width [1..]) (replicate height [1..])

  putStrLn "\nExo 2:"
