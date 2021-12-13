module Aoc where

import Data.List
import Data.List.Split (splitOn)

loadLines :: String -> IO [String]
loadLines filePath = do
  fileCont <- readFile filePath
  return $ lines fileCont

loadCsv :: String -> IO [String]
loadCsv filePath = do
  fileCont <- readFile filePath
  return $ splitOn "," fileCont

loadCsvInts :: String -> IO [Int]
loadCsvInts filePath = do
  fileCont <- readFile filePath
  return $ map (read :: (String -> Int)) (splitOn "," fileCont)

calcDistance :: Int -> [Int] -> Int
calcDistance point = foldl (\acc pos -> acc + sum [1 .. abs (pos - point)]) 0

mode :: [Int] -> Int
mode positions = sort positions !! (length positions `div` 2)

findMin :: [Int] -> (Int, Int)
findMin positions = findMin' cur curDistance positions
  where
    cur = mode positions
    curDistance = calcDistance cur positions

findMin' :: Int -> Int -> [Int] -> (Int, Int)
findMin' cur curDistance positions
  | prev >= curDistance && next >= curDistance = (cur, curDistance)
  | prev < curDistance = findMin' (cur - 1) prev positions
  | otherwise = findMin' (cur + 1) next positions
  where
    prev = calcDistance (cur - 1) positions
    next = calcDistance (cur + 1) positions

solve :: [String] -> Int
solve fileLines = 0
