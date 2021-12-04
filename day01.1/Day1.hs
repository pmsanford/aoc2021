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

main :: IO ()
main = do
  file_cont <- readFile "main.input"
  let count = countFileIncreases file_cont
  print count
