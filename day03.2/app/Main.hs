-- AOC 2021 Day 3

import Data.Char
import Data.List
import System.Environment

bitsToInt :: [Int] -> Int
bitsToInt = foldl' (\acc x -> acc * 2 + x) 0

listToBits :: String -> [Int]
listToBits = map digitToInt

partitionAt :: Int -> [[Int]] -> ([[Int]], [[Int]])
partitionAt idx = partition (\x -> x !! idx == 0)

type Cmp = (Int -> Int -> Bool)

prefCmp :: Int -> Cmp -> Int -> ([[Int]], [[Int]]) -> [[Int]]
prefCmp idx cmp pref (x1, x2)
  | x1l == x2l = pickPref
  | cmp x1l x2l = x1
  | otherwise = x2
  where
    x1l = length x1
    x2l = length x2
    pickPref = if head x1 !! idx == pref then x1 else x2

filterAt :: Int -> Cmp -> Int -> [[Int]] -> [[Int]]
filterAt idx cmp pref xs = prefCmp idx cmp pref (partitionAt idx xs)

winnow :: Int -> Cmp -> [[Int]] -> [Int]
winnow = winnow' 0

winnow' :: Int -> Int -> Cmp -> [[Int]] -> [Int]
winnow' idx pref cmp xs
  | length xs == 1 = head xs
  | otherwise = winnow' (idx + 1) pref cmp (filterAt idx cmp pref xs)

loadRun :: String -> IO ()
loadRun filePath = do
  fileCont <- readFile filePath
  let fileLines = lines fileCont
  let o = map listToBits fileLines
  let oxy = bitsToInt $ winnow 1 (>) o
  let co2 = bitsToInt $ winnow 0 (<) o
  print oxy
  print co2
  print (oxy * co2)

main :: IO ()
main = do
  args <- getArgs
  loadRun (head args)
