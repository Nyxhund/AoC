module Main where

import Data.List
import Data.List.Split
import System.IO
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()
import Debug.Trace

matchedOrNot :: Maybe [String] -> [String]
matchedOrNot Nothing = []
matchedOrNot (Just a) = a

part1 :: String -> Int
part1 text = sum $ map ((\[a, b] -> a * b) . map read . splitOn "," . takeWhile (/=')') . drop 4) matches
        where reg = "mul\\([0-9]{1,3},[0-9]{1,3}\\)"
              matches = getAllTextMatches (text =~ reg) :: [String]

takeWhileString :: String -> String -> String
takeWhileString _ [] = []
takeWhileString pattern rest@(a:ta) =
                        if pattern `isPrefixOf` rest then
                            []
                        else
                            a : takeWhileString pattern ta

parse :: String -> [String]
parse s
    | "do()" `isPrefixOf` s    = keep : parse rDo
    | "don't()" `isPrefixOf` s = parse rDont
    | "" == s                  = []
        where keep = takeWhileString "don't()" s
              rDo = drop (length keep) s
              keepDont = takeWhileString "do()" s
              rDont = drop (length keepDont) s

part2 :: String -> Int
part2 = sum . map part1 . parse

main = do
  file <- readFile "input.txt"
  -- file <- readFile "example.txt"

  putStrLn "Exo 1:"
  print $ part1 file

  putStrLn "\nExo 2:"
  print $ part2 ("do()" ++ file)
