-- AOC 2021 Day 3
import System.Environment
import Data.List
import Data.Char

parseBin :: String -> Int
parseBin = foldl' (\acc x -> acc * 2 + digitToInt x) 0

listToBin :: [Int] -> Int
listToBin = foldl' (\acc x -> acc * 2 + x) 0

posSums :: [String] -> [Int]
posSums fileLines = map sum (transpose (map (map digitToInt) fileLines))

gamma :: [String] -> Int 
gamma fileLines = listToBin (map (\v -> if v > div (length fileLines) 2 then 1 else 0) (posSums fileLines))

epsilon :: [String] -> Int 
epsilon fileLines = listToBin (map (\v -> if v <= div (length fileLines) 2 then 1 else 0) (posSums fileLines))

loadRun :: String -> IO ()
loadRun filePath = do
  fileCont <- readFile filePath
  let fileLines = lines fileCont
  let g = gamma fileLines
  let e = epsilon fileLines
  print (g * e)

main :: IO ()
main = do
  args <- getArgs
  loadRun (head args)

