-- AOC 2021 Day 2
import System.Environment
import Data.List

parseSnd :: [String] -> (String, Int)
parseSnd xs = (head xs, (read::String->Int) (head (tail xs)))

advanceSub :: (String, Int) -> (Int, Int) -> (Int, Int)
advanceSub cmd position
  | fst cmd == "forward" = (fst position + snd cmd, snd position)
  | fst cmd == "down" = (fst position, snd position + snd cmd)
  | fst cmd == "up" = (fst position, snd position - snd cmd)
  | otherwise = position

moveSub' :: [(String, Int)] -> (Int, Int) -> (Int, Int)
moveSub' xs pos
  | null xs = pos
  | otherwise = moveSub' (tail xs) (advanceSub (head xs) pos)

moveSub :: [(String, Int)] -> (Int, Int)
moveSub xs = moveSub' xs (0, 0)

loadRun :: String -> IO ()
loadRun filePath = do
  fileCont <- readFile filePath
  let fileLines = lines fileCont
  let cmds = map (parseSnd . words) fileLines
  let pos = moveSub cmds
  let result = uncurry (*) pos
  print result

main :: IO ()
main = do
  args <- getArgs
  loadRun (head args)

