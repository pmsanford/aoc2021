module Aoc where

import Data.List.Split

countSegs :: [String] -> Int
countSegs lns = length $ filter (\x -> length x < 5 || length x == 7) (concatMap (words . (!! 1) . splitOn "|") lns)

loadLines :: String -> IO [String]
loadLines filePath = do
  fileCont <- readFile filePath
  return $ lines fileCont

loadRun :: String -> IO ()
loadRun filePath = do
  fileLines <- loadLines filePath
  print $ Aoc.solve fileLines

solve :: [String] -> Int
solve = countSegs
