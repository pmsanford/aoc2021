{-# LANGUAGE TupleSections #-}

-- AOC 2021 Day 4

import qualified Data.IntSet as Set
import Data.List
import Data.List.Split (chunksOf, splitOn)
import Data.Maybe
import System.Environment

type Board = ([[Cell]], Maybe Int)

type Cell = (Int, Bool)

boardToInt :: [String] -> [[Int]]
boardToInt = map (map (read :: (String -> Int)) . words)

toCells :: [Int] -> [Cell]
toCells = map (,False)

markCell :: Int -> Cell -> Cell
markCell draw (cont, val)
  | draw == cont = (cont, True)
  | otherwise = (cont, val)

markBoard :: Int -> Board -> Board
markBoard draw (cells, won)
  | isJust won = (cells', won)
  | isWinner (cells', won) = (cells', Just draw)
  | otherwise = (cells', Nothing)
  where
    cells' = map (map (markCell draw)) cells

markCells :: Int -> [Board] -> [Board]
markCells draw = map (markBoard draw)

isWinner :: Board -> Bool
isWinner (cells, _) = isWinner' cells || isWinner' (transpose cells)
  where
    isWinner' = any (all snd)

createCells :: [String] -> [[[Cell]]]
createCells xs = map (map toCells . boardToInt) (chunksOf 5 (filterSpaces xs))
  where
    filterSpaces = filter (/= "")

makeBoard :: [[Cell]] -> Board
makeBoard = (,Nothing)

parseBoards :: [String] -> ([Int], [Board])
parseBoards (numbers : xs) = (map (read :: (String -> Int)) (splitOn "," numbers), map makeBoard (createCells xs))
parseBoards [] = ([], [])

firstWinner :: [Int] -> [Board] -> Maybe (Int, Board)
firstWinner (curr : numbers) boards
  | allWon = Just (curr, fromJust winner)
  | otherwise = firstWinner numbers updated
  where
    updated = markCells curr boards
    winner = find (\(_, x) -> x == Just curr) updated
    allWon = all (\(_, x) -> isJust x) updated
firstWinner [] _ = Nothing

calcScore :: (Int, Board) -> Int
calcScore (num, board) = num * sum (map fst (filter (not . snd) (concat (fst board))))

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
