-- AOC 2021 Day 2

import Data.List
import System.Environment

parseSnd :: [String] -> (String, Int)
parseSnd xs = (head xs, (read :: String -> Int) (head (tail xs)))

advanceSub :: (String, Int) -> (Int, Int, Int) -> (Int, Int, Int)
advanceSub cmd (aim, xpos, ypos)
  | fst cmd == "forward" = (aim, xpos + snd cmd, ypos + (aim * snd cmd))
  | fst cmd == "down" = (aim + snd cmd, xpos, ypos)
  | fst cmd == "up" = (aim - snd cmd, xpos, ypos)
  | otherwise = (aim, xpos, ypos)

moveSub' :: [(String, Int)] -> (Int, Int, Int) -> (Int, Int)
moveSub' xs (aim, xpos, ypos)
  | null xs = (xpos, ypos)
  | otherwise = moveSub' (tail xs) (advanceSub (head xs) (aim, xpos, ypos))

moveSub :: [(String, Int)] -> (Int, Int)
moveSub xs = moveSub' xs (0, 0, 0)

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
