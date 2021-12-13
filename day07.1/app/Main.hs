-- AOC 2021 Day 7
import System.Environment
import Data.List

import Aoc

loadRun :: String -> IO ()
loadRun filePath = do
  positions <- loadCsvInts filePath
  print $ findMin positions
  
main :: IO ()
main = do
  args <- getArgs 
  loadRun (head args)

