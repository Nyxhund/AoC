module Main where

import Data.Char
import Data.List
import System.IO
import Data.List.Split
import Debug.Trace
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Data.Function.Memoize

parse :: String -> [String]
parse str = map c (head d) : tail d
        where d = lines str
              c 'S' = '|'
              c ch  = ch

createLine :: Int -> (Set.Set Int, Set.Set Int) -> String
createLine size (indexes, blanks) = map c $ take size [0..]
        where c i | i `elem` indexes = '|'
                  | i `elem` blanks  = ' '
                  | otherwise        = '.'

traverse :: ((Set.Set Int, Set.Set Int) -> String) -> [String] -> [String]
traverse _ [str]    = [str]
traverse createLine (source : target : ts) = line : Main.traverse createLine (line : ts)
    where
        m (s, b) (i, '|', '.') = (Set.insert i s, b)
        m (s, b) (i, '|', '^') = (Set.insert (i-1) $ Set.insert (i+1) s, Set.insert i b)
        m (s, b) _ = (s, b)
        set = foldl m (Set.empty, Set.empty) (zip3 [0..] source target)
        line = createLine set

part1 :: [String] -> Int
part1 d = sum $ map (length . filter (==' ')) traversed
        where traversed = Main.traverse createLine' d
              createLine' = createLine (length (head d))

f :: IntMap.IntMap [Int] -> (Int, Int) -> Int
f m = memoFix recur
        where recur r (i, j) = query (i-1, j) + query (i+1, j)
                    where query (x, y) = case IntMap.lookup x m of
                                            Nothing -> 1
                                            Just l  -> case find (>j) l of
                                                        Nothing -> 1
                                                        Just w  -> r (x, w)

part2 :: [String] -> Int
part2 d = f m (70, 1)  -- Position of the S, I was losing my mind, could have
                       -- been in code, sorry ! ;)
        where traversed = Main.traverse createLine' d
              createLine' = createLine (length (head d))
              transposed = transpose traversed
              m = IntMap.fromList (zip [0..] (map (map fst . filter ((==' ') . snd) . zip [0..]) transposed))

main = do
  putStrLn "Exo 1:"
  file <- readFile "input.txt"
  -- file <- readFile "example.txt"

  let d = parse file
  print $ part1 d

  putStrLn "Exo 2:"
  print $ part2 d
