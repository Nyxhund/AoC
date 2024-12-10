module Main where

import Data.List
import System.IO
import Data.Char
import Debug.Trace

calculateChecksum :: [Int] -> Int
calculateChecksum = sum . zipWith (*) [0..] . map (\x -> if x == -1 then 0 else x)

parse :: String -> [(Int, Int)]
parse = flip zip (interleave [0..] (repeat (-1))) . map digitToInt . init
            where interleave xs ys = concat (transpose [xs, ys])

compressData :: [(Int, Int)] -> [(Int, Int)] -> Int -> [(Int, Int)]
compressData _ _ 0 = []
compressData current ((nb, -1) : ta) handled = trace ("Removing " ++ show nb ++ " on the left") compressData current ta (max 0 (handled - nb))
compressData (elt@(currNb, currId) : tCurr) inversed@((invNb, invId) : tInv) handled
        | currId /= -1                    = trace ("Handled curr is good " ++ show elt) elt : compressData tCurr inversed handled
        | currId == -1 && currNb == invNb = trace ("Handled space is just right, handled is now " ++ show (handled - currNb) ++ " removing " ++ show currNb) (invNb, invId) : compressData tCurr tInv (max 0 (handled - currNb))
        | currId == -1 && currNb < invNb  = trace ("Handled curr is lower, handled is now " ++ show (handled - currNb)) (currNb, invId) : compressData tCurr ((invNb - currNb, invId) : tInv) (max 0 (handled - currNb))
        | currId == -1 && currNb > invNb  = trace ("Handled curr is higher, handled is now " ++ show (handled - invNb))(invNb, invId) : compressData ((currNb - invNb, currId) : tCurr) tInv (max 0 (handled - invNb))

toExtended :: [(Int, Int)] -> [Int]
toExtended = concatMap (uncurry replicate)

part1 :: [(Int, Int)] -> Int
part1 input = calculateChecksum $ take proper extended
    where reversed = reverse input
          toHandle = sum $ map fst $ filter (\(_, id) -> id == -1) input
          proper = sum $ map fst $ filter (\(_, id) -> id /= -1) input
          extended = toExtended $ compressData input reversed (trace (show toHandle) (toHandle + 1))

removeSame :: Int -> [(Int, Int)] -> [(Int, Int)]
removeSame _ [] = []
removeSame a (curr@(nb, id) : tl)
        | a == id     = (nb, -1) : tl
        | otherwise   = curr : removeSame a tl

fuseEmpty :: [(Int, Int)] -> [(Int, Int)]
fuseEmpty ((a, -1) : (b, -1) : ta) = fuseEmpty ((a + b, -1) : ta)
fuseEmpty a = a

compressFile :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
compressFile toInsert@(nb, id) l@(current@(nbCurr, idCurr) : tlist)
        | id == idCurr                 = l
        | idCurr == -1 && nb == nbCurr = toInsert : fuseEmpty ((nbCurr - nb, -1) : removeSame id tlist)
        | idCurr == -1 && nb < nbCurr  = toInsert : fuseEmpty ((nbCurr - nb, -1) : removeSame id tlist)
        | otherwise                    = current : compressFile toInsert tlist

part2 :: [(Int, Int)] -> Int
part2 input = (calculateChecksum . toExtended) compressed
    where reversed = filter (\(_, id) -> id /= -1) input
          compressed = foldr compressFile input reversed

main = do
  file <- readFile "input.txt"
  -- file <- readFile "example.txt"
  let input = parse file

  putStrLn "Exo 1:"
  print $ part1 input

  putStrLn "\nExo 2:"
  print $ part2 input
