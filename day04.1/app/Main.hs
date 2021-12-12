{-# LANGUAGE TupleSections #-}

-- AOC 2021 Day 4

import qualified Data.IntSet as Set
import Data.List
import Data.List.Split (chunksOf, splitOn)
import Data.Maybe
import System.Environment

type Board = [[Cell]]

type Cell = (Int, Bool)

boardToInt :: [String] -> [[Int]]
boardToInt = map (map (read :: (String -> Int)) . words)

toCells :: [Int] -> [Cell]
toCells = map (,False)

markCell :: Int -> Cell -> Cell
markCell draw (cont, val)
  | draw == cont = (cont, True)
  | otherwise = (cont, val)

markCells :: Int -> [Board] -> [Board]
markCells draw = map (map (map (markCell draw)))

isWinner :: Board -> Bool
isWinner board = isWinner' board || isWinner' (transpose board)
  where
    isWinner' = any (all snd)

parseBoards :: [String] -> ([Int], [Board])
parseBoards (numbers : xs) = (map (read :: (String -> Int)) (splitOn "," numbers), map (map toCells . boardToInt) (chunksOf 5 (filterSpaces xs)))
  where
    filterSpaces = filter (/= "")
parseBoards [] = ([], [])

firstWinner :: [Int] -> [Board] -> Maybe (Int, Board)
firstWinner (curr : numbers) boards
  | isJust winner = Just (curr, fromJust winner)
  | otherwise = firstWinner numbers updated
  where
    updated = markCells curr boards
    winner = find isWinner updated
firstWinner [] _ = Nothing

calcScore :: (Int, Board) -> Int
calcScore (num, board) = num * sum (map fst (filter (not . snd) (concat board)))

loadBoards :: String -> IO ([Int], [Board])
loadBoards filePath = do
  fileLines <- loadLines filePath
  return $ parseBoards fileLines

loadLines :: String -> IO [String]
loadLines filePath = do
  fileCont <- readFile filePath
  return $ lines fileCont

loadRun :: String -> IO ()
loadRun filePath = do
  (numbers, boards) <- loadBoards filePath
  let winner = fromJust $ firstWinner numbers boards
  let result = calcScore winner
  print result

main :: IO ()
main = do
  args <- getArgs
  loadRun (head args)
