-- AOC 2021 Day 7

import Aoc
import Data.List
import System.Environment

loadRun :: String -> IO ()
loadRun filePath = do
  positions <- loadCsvInts filePath
  print $ findMin positions

main :: IO ()
main = do
  args <- getArgs
  loadRun (head args)
