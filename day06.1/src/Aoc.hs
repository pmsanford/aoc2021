module Aoc where
import Data.List.Split

type Fish = Int

initFish :: [String] -> [Fish]
initFish xs = map (read::(String->Int)) (splitOn "," (head xs))

tick :: [Fish] -> [Fish]
tick (cur : xs)
  | cur == 0 = 6 : 8 : tick xs
  | otherwise = (cur - 1) : tick xs
tick [] = []

tickFor :: Int -> [Fish] -> [Fish]
tickFor 0 fish = fish
tickFor days fish = tickFor (days - 1) (tick fish)

solve :: [String] -> Int
solve fileLines = length $ Aoc.tickFor 80 (initFish fileLines)

