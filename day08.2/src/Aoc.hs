module Aoc where

import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Maybe

type Sieve = [String] -> String

type Decoder = Map.Map String String

filterBy :: (String -> Bool) -> [String] -> String
filterBy f wires = sort $ head (filter f wires)

byLength :: Int -> [String] -> String
byLength len = filterBy (\x -> length x == len)

one :: Sieve
one = byLength 2

two :: Sieve
two wires = filterBy (\x -> length x == 5 && length (four wires `intersect` x) == 2) wires

three :: Sieve
three wires = filterBy (\x -> length x == 5 && length (seven wires `intersect` x) == 3) wires

four :: Sieve
four = byLength 4

five :: Sieve
five wires = filterBy (\x -> length x == 5 && length (four wires `intersect` x) == 3 && length (seven wires `intersect` x) == 2) wires

six :: Sieve
six wires = filterBy (\x -> length x == 6 && length (one wires `intersect` x) == 1) wires

seven :: Sieve
seven = byLength 3

eight :: Sieve
eight = byLength 7

nine :: Sieve
nine wires = filterBy (\x -> length x == 6 && length (four wires `intersect` x) == 4) wires

zero :: Sieve
zero wires = filterBy (\x -> length x == 6 && length (five wires `intersect` x) == 4) wires

buildDecoder :: [String] -> Decoder
buildDecoder wires = Map.fromList [(zero wires, "0"), (one wires, "1"), (two wires, "2"), (three wires, "3"), (four wires, "4"), (five wires, "5"), (six wires, "6"), (seven wires, "7"), (eight wires, "8"), (nine wires, "9")]

loadLines :: String -> IO [String]
loadLines filePath = do
  fileCont <- readFile filePath
  return $ lines fileCont

loadWires :: String -> IO [([String], [String])]
loadWires filePath = do
  fileLines <- loadLines filePath
  return $ map ((\[a, b] -> (words a, words b)) . splitOn "|") fileLines

loadRun :: String -> IO ()
loadRun filePath = do
  input <- loadWires filePath
  print $ Aoc.solve input

solveLine :: ([String], [String]) -> Int
solveLine (wires, disp) = buildInt $ map ((`Map.lookup` decoder) . sort) disp
  where
    decoder = buildDecoder wires
    buildInt justs = (read :: (String -> Int)) (concatMap fromJust justs)

solve :: [([String], [String])] -> Int
solve inputs = sum $ map solveLine inputs
