module Main where

import Data.Char
import Data.List
import System.IO
import Data.List.Split
import Debug.Trace
import Math.LinearEquationSolver

data Machine = Machine
  { target    :: [Bool]
  , buttons   :: [[Int]]
  , joltage   :: [Int]
  } deriving (Show, Eq)

type Transition a = a -> a
type Button a = [Transition a]
type State a = [a]

parse :: String -> [Machine]
parse str = map h d
        where d = map words $ lines str
              h l = Machine target buttons joltage
                    where target  = map (=='#') $ takeWhile (/=']') $ tail $ head l
                          buttons = map (read . map m) $ takeWhile ((/='{') . head) $ tail l
                          joltage = (read . map m) $ last l
                          m '(' = '['
                          m ')' = ']'
                          m '{' = '['
                          m '}' = ']'
                          m c   = c


pressButton :: Eq a => Button a -> State a -> State a
pressButton b s = map (\(transition, bit) -> transition bit) $ zip b s

genPossibilities :: Eq a => [Button a] -> State a -> [State a]
genPossibilities buttons s = zipWith pressButton buttons (repeat s)

recur :: (Eq a, Ord a) => State a -> [Button a] -> Int -> [State a] -> Int
recur target func i list | target `elem` next = i
                              | otherwise          = recur target func (i+1) next
      where next = concatMap (genPossibilities func) (nub list)

makeTransitions :: (Bool -> (a -> a)) -> Int -> [[Int]] -> [[a -> a]]
makeTransitions f l buttons = map (\(i, l) -> map (\x -> f (x `elem` i)) l) $ zip buttons (repeat [0 .. (l - 1)])

findSmallest :: Machine -> Int
findSmallest (Machine target buttons _) = recur target func 1 initial
        where initial = [replicate l False]
              l = length target
              func = makeTransitions f l buttons
                    where f True  = not
                          f False = id

part1 :: [Machine] -> Int
part1 = sum . map findSmallest

findSmallestJoltage :: Machine -> Int
findSmallestJoltage (Machine _ buttons target) = recur target func 1 initial
        where initial = [replicate l 0]
              l = length target
              func = makeTransitions f l buttons
                    where f True  = (+1)
                          f False = id

makeMatrix :: Int -> [[Int]] -> [[Integer]]
makeMatrix l buttons = map (\(i, l) -> map (\x -> f (x `elem` i)) l) $ zip buttons (repeat [0 .. (l - 1)])
            where f True  = 1
                  f False = 0

part2 :: [Machine] -> Int
part2 = sum . map findSmallestJoltage

main = do
  putStrLn "Exo 1:"
  -- file <- readFile "input.txt"
  file <- readFile "example.txt"

  let d = parse file
  -- mapM_ print d
  print $ part1 d
  -- print $ findSmallest $ head $ tail $ tail d

  let mat = makeMatrix 4 $ buttons $ head d
  -- mapM_ print test
  res <- solveIntegerLinearEqsAll Z3 5 (transpose mat) [3, 5, 4, 7]
  print res
  -- print $ part2 d
