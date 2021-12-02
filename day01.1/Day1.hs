import GHC.IO.IOMode (IOMode(ReadMode))
import System.IO (openFile, putStrLn)
import Prelude hiding (putStrLn)

countIncrease :: Int -> [Int] -> Int -> Int
countIncrease prev xs count = if head xs > prev then count + 1 else count

countIncreases :: [Int] -> Int
countIncreases xs = countIncreasesI (head xs) (tail xs) 0

countIncreasesI :: Int -> [Int] -> Int -> Int
countIncreasesI prev xs count = if null xs then count else countIncreasesI (head xs) (tail xs) (countIncrease prev xs count)

countFileIncreases :: String -> Int
countFileIncreases xs = countIncreases (map (read::String->Int) (lines xs))

main :: IO ()
main = do
  file_cont <- readFile "input.txt"
  let count = countFileIncreases file_cont
  print count
