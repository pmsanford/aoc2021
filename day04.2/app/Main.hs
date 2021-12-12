{-# LANGUAGE TupleSections #-}
-- AOC 2021 Day 4
import System.Environment
import Data.List
import Data.List.Split (splitOn, chunksOf)
import qualified Data.IntSet as Set
import Data.Maybe

type Board = ([[Cell]], Bool)
type Cell = (Int, Bool)

boardToInt :: [String] -> [[Int]]
boardToInt = map (map (read::(String->Int)) . words)

toCells :: [Int] -> [Cell]
toCells = map (, False)

markCell :: Int -> Cell -> Cell
markCell draw (cont, val)
  | draw == cont = (cont, True)
  | otherwise = (cont, val)

markBoard :: Int -> Board -> Board
markBoard draw (cells, won)
  | won = (cells', won)
  | otherwise = (cells', isWinner (cells', won))
  where cells' = map (map (markCell draw)) cells

markCells :: Int -> [Board] -> [Board]
markCells draw = map (markBoard draw)

isWinner :: Board -> Bool
isWinner (cells, _) = isWinner' cells || isWinner' (transpose cells)
  where isWinner' = any (all snd)

createCells :: [String] -> [[[Cell]]]
createCells xs = map (map toCells . boardToInt) (chunksOf 5 (filterSpaces xs))
  where filterSpaces = filter (/= "")

makeBoard :: [[Cell]] -> Board
makeBoard = (, False)

parseBoards :: [String] -> ([Int], [Board])
parseBoards (numbers : xs) = (map (read::(String->Int)) (splitOn "," numbers), map makeBoard (createCells xs))

parseBoards [] = ([], [])

firstWinner :: [Int] -> [Board] -> Maybe (Int, Board)
firstWinner (curr : numbers) boards
  | isJust winner = Just (curr, fromJust winner)
  | otherwise = firstWinner numbers updated
  where updated = markCells curr boards
        winner = find isWinner updated

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
  print winner
  let result = calcScore winner
  print result

main :: IO ()
main = do
  args <- getArgs
  loadRun (head args)

