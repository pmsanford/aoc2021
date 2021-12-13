-- AOC 2021 Day 6

import Aoc
import Data.List
import System.Environment

loadLines :: String -> IO [String]
loadLines filePath = do
  fileCont <- readFile filePath
  return $ lines fileCont

loadRun :: String -> IO ()
loadRun filePath = do
  fileLines <- loadLines filePath
  print $ Aoc.solve fileLines

main :: IO ()
main = do
  args <- getArgs
  loadRun (head args)
