module Main where

import Data.Char
import Data.List
import System.IO
import Data.List.Split
import Debug.Trace
import Data.SBV
import Data.Foldable
import Data.Traversable
import Data.Maybe

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

-- This is very much not my answer, but I legit did not know how to use SBV
-- correctly, so I will assume this is a learning moment and it counts
-- Thanks Sheinxy !! If you wanna read pretty Haskell, go to
-- https://github.com/Sheinxy/Advent-Of-Code/
findBestCombination :: Machine -> IO SMTResult
findBestCombination (Machine _ buttons target) = optLexicographic $ do
        ts <- sIntegers ["t" ++ show n | n <- [0 .. length buttons - 1]]
        for_ ts $ \t -> do
            constrain $ t .>= 0
        for_ (zip [0 ..] target) $ \(i, jolt) -> do
            constrain $ fromIntegral jolt .== sum [t | (t, b) <- zip ts buttons, i `elem` b]
        minimize "total" (sum ts)

part2 :: [Machine] -> IO Int
part2 input = do
        opts <- mapM findBestCombination input
        mins <- for opts $ \res -> do
                let x = fromJust $ getModelValue "total" res :: Int64
                return $ fromIntegral x
        return $ sum mins

main = do
  putStrLn "Exo 1:"
  file <- readFile "input.txt"
  -- file <- readFile "example.txt"

  let d = parse file
  print $ part1 d

  res <- part2 d
  print res
