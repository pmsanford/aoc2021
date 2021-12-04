import GHC.IO.IOMode (IOMode (ReadMode))
import System.IO (openFile, putStrLn)
import Prelude hiding (putStrLn)

countIncrease :: Int -> Int -> Int -> Int
countIncrease prev cur count
  | cur > prev = count + 1
  | otherwise = count

countIncreases :: [Int] -> Int
countIncreases [] = 0
countIncreases (cur : xs) = countIncreasesI cur xs 0

countIncreasesI :: Int -> [Int] -> Int -> Int
countIncreasesI prev [] count = count
countIncreasesI prev (cur : xs) count = countIncreasesI cur xs (countIncrease prev cur count)

countFileIncreases :: String -> Int
countFileIncreases xs = countIncreases (map (read :: String -> Int) (lines xs))

sumWindows :: [Int] -> [Int]
sumWindows xs = reverse (sumWindows' xs [])

sumWindows' :: [Int] -> [Int] -> [Int]
sumWindows' xs agg
  | length xs < 3 = agg
  | otherwise = sumWindows' (tail xs) (sum (take 3 xs) : agg)

main :: IO ()
main = do
  file_cont <- readFile "main.input"
  let count = countIncreases (sumWindows (map (read :: String -> Int) (lines file_cont)))
  print count
