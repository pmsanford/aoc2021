-- AOC 2021 Day 9
import System.Environment
import Data.List

import Aoc

loadRun :: String -> IO ()
loadRun filePath = do
  fileLines <- loadGrid filePath
  print $ Aoc.solve fileLines
  
main :: IO ()
main = do
  args <- getArgs 
  loadRun (head args)

