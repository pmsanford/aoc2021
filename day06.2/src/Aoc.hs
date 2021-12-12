module Aoc where
import Data.List.Split

type School = [Int]

newSchool :: School
newSchool = [0, 0, 0, 0, 0, 0, 0, 0, 0]

initFish :: [String] -> [Int]
initFish xs = map (read::(String->Int)) (splitOn "," (head xs))

initSchool' :: [Int] -> School -> School
initSchool' (cur : xs) school = initSchool' xs (addOne (splitAt (8 - cur) school))
        where addOne (x, y:ys) = x ++ (y + 1) : ys
initSchool' [] school = school

initSchool :: [String] -> School
initSchool fileLines = initSchool' (initFish fileLines) newSchool


tick :: School -> School
tick [g8, g7, g6, g5, g4, g3, g2, g1, g0] = [g0, g8, g7 + g0, g6, g5, g4, g3, g2, g1]
tick _ = error "bad school"

tickFor :: Int -> School -> School
tickFor 0 school = school
tickFor days school = tickFor (days - 1) (tick school)

solve :: [String] -> Int
solve fileLines = sum $ Aoc.tickFor 256 (initSchool fileLines)

