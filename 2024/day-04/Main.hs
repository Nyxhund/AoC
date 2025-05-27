module Main where

import Data.List
import System.IO
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()
import Data.Matrix (fromLists, getElem, Matrix, nrows, ncols)

diagonal :: [String] -> [String]
diagonal mat = transpose $ map (\(i, l) -> replicate i '.' ++ l ++ replicate (len - i - 1) '.') (zip (iterate (+1) 0) mat)
        where len = length $ head mat

parse :: String -> [[String]]
parse text = [origin, transposed, diagonal origin, diagonal reversed]
            where origin = lines text
                  transposed = transpose origin
                  reversed = map reverse origin

part1 :: [[String]] -> [[Int]]
part1 = map (map matchRegex)
        where reg = "XMAS"
              otherreg = "SAMX"
              matchRegex x = length $ (getAllTextMatches (x =~ reg) :: [String]) ++ (getAllTextMatches (x =~ otherreg) :: [String])

-- matchElemVertical :: Int -> Int -> Matrix Char -> Bool
-- matchElemVertical a b mat = (getElem (a - 1) b mat == 'S' && getElem (a + 1) b mat == 'M') ||
--             (getElem (a + 1) b mat == 'S' && getElem (a - 1) b mat == 'M')
--
-- matchElemHorizontal :: Int -> Int -> Matrix Char -> Bool
-- matchElemHorizontal a b mat =
--         (getElem a (b + 1) mat == 'S' && getElem a (b - 1) mat == 'M') ||
--          (getElem a (b - 1) mat == 'S' && getElem a (b + 1) mat == 'M')

matchElemDiagonalRight :: Int -> Int -> Matrix Char -> Bool
matchElemDiagonalRight a b mat =
        (getElem (a - 1) (b - 1) mat == 'S' && getElem (a + 1) (b + 1) mat == 'M') ||
         (getElem (a + 1) (b + 1) mat == 'S' && getElem (a - 1) (b - 1) mat == 'M')

matchElemDiagonalLeft :: Int -> Int -> Matrix Char -> Bool
matchElemDiagonalLeft a b mat =
        (getElem (a - 1) (b + 1) mat == 'S' && getElem (a + 1) (b - 1) mat == 'M') ||
         (getElem (a + 1) (b - 1) mat == 'S' && getElem (a - 1) (b + 1) mat == 'M')

matchElemFromMat :: Int -> Int -> Matrix Char -> Bool
matchElemFromMat a b mat = matchElemDiagonalLeft a b mat && matchElemDiagonalRight a b mat

traverseMatrix :: Int -> Int -> [String] -> Matrix Char -> Int
traverseMatrix a b ([] : ta) mat = traverseMatrix 1 (b + 1) ta mat
traverseMatrix _ _ [] _ = 0
traverseMatrix a b rest@(l : tl) mat =
            if c == 'A' && a /= 1 && b /= 1 && a /= ncols mat && b /= nrows mat && matchElemFromMat b a mat then
                1 + traverseMatrix (a + 1) b (drop 1 l : tl) mat
            else
                0 + traverseMatrix (a + 1) b (drop 1 l : tl) mat
            where c = head $ head rest

part2 :: [String] -> Int
part2 mat = traverseMatrix 1 1 mat m
        where m = fromLists mat

main = do
  file <- readFile "input.txt"
  -- file <- readFile "example.txt"
  let mat = parse file

  -- traverse print (head mat)
  -- print "---------------"
  -- traverse print (head $ drop 1 mat)
  -- print "---------------"
  -- traverse print (head $ drop 2 mat)
  -- print "---------------"
  -- traverse print (head $ drop 3 mat)
  -- print "---------------"

  putStrLn "Exo 1:"
  let res = part1 mat
  print res
  print $ map sum res
  print $ sum $ map sum res

  putStrLn "\nExo 2:"
  -- 2790 too high
  print $ part2 $ head mat
