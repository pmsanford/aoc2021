{-# LANGUAGE TupleSections #-}
-- AOC 2021 Day 5
import System.Environment
import Data.List
import Text.Regex.TDFA
import qualified Data.Map.Strict as Map
import Data.Maybe

type Vent = ((Int, Int), (Int, Int))

parseVent :: String -> Vent
parseVent ln = mkTuples (map (read::(String->Int)) (getCaptures matchLine))
  where getCaptures (_, _, _, captures) = captures
        matchLine = ln =~ "([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)" :: (String, String, String, [String])
        mkTuples [x1, y1, x2, y2] = ((x1, y1), (x2, y2))
        mkTuples _ = error "Wrong number of matches"

genPoints :: Vent -> [(Int, Int)]
genPoints ((x1, y1), (x2, y2))
  | x1 == x2 && y1 < y2 = map (x1,) [y1..y2]
  | x1 == x2 = map (x1,) [y2..y1]
  | y1 == y2 && x1 < x2 = map (,y1) [x1..x2]
  | y1 == y2 = map (,y1) [x2..x1]
  | otherwise = []

walkVents :: [Vent] -> Map.Map (Int, Int) Int
walkVents = walkVents' Map.empty

addOne :: Maybe Int -> Maybe Int
addOne mv = Just (maybe 1 (+ 1) mv)

walkVents' :: Map.Map (Int, Int) Int -> [Vent] -> Map.Map (Int, Int) Int
walkVents' m (c : xs) = walkVents' (addPoints m (genPoints c)) xs
  where addPoints mm (cc : xss) = addPoints (Map.alter addOne cc mm) xss
        addPoints mm [] = mm
walkVents' m [] = m

countMultiPoints :: Map.Map (Int, Int) Int -> Int
countMultiPoints m = length $ Map.filterWithKey gt m
  where gt _ v = v > 1

loadVents :: String -> IO [Vent]
loadVents filePath = do
  fileCont <- readFile filePath
  return $ map parseVent (lines fileCont)

loadRun :: String -> IO ()
loadRun filePath = do
  vents <- loadVents filePath
  let field = walkVents vents
  print $ countMultiPoints field

main :: IO ()
main = do
  args <- getArgs
  loadRun (head args)

