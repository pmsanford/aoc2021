{-# LANGUAGE TupleSections #-}
module Aoc where
import Data.List.Split
import Data.Maybe
import Data.List

-- pass across marking candidates
-- transpose
-- another pass across marking candidates
-- collect

-- maybe greater than
mgt :: Ord a => Maybe a -> a -> Bool
mgt a b = maybe True (> b) a

markCandidates :: [Cell] -> [Cell]
markCandidates (curr : next : cells) = markCandidates' Nothing curr (Just next) cells
markCandidates _ = error "Bad grid"


markCandidates' :: Maybe Cell -> Cell -> Maybe Cell -> [Cell] -> [Cell]
markCandidates' Nothing (curr, cmark) (Just (next, nmark)) (nnext : xs) = (curr, cmark && curr < next) : markCandidates' (Just (curr, cmark)) (next, nmark) (Just nnext) xs
markCandidates' (Just (prev, pmark)) (curr, cmark) (Just (next, nmark)) [] = (curr, cmark && (curr < next && curr < prev)) : markCandidates' (Just (curr, cmark)) (next, nmark) Nothing []
markCandidates' (Just (prev, pmark)) (curr, cmark) (Just (next, nmark)) (nnext : xs) = (curr, cmark && (curr < next && curr < prev)) : markCandidates' (Just (curr, cmark)) (next, nmark) (Just nnext) xs
markCandidates' (Just (prev, pmark)) (curr, cmark) Nothing [] = [(curr, cmark && (curr < prev))]
markCandidates' _ _ _ _ = error "Impossible!"

type Cell = (Int, Bool)

loadGrid :: String -> IO [[Cell]]
loadGrid filePath = do
  lns <- loadLines filePath
  let ints = map (map (read :: (String -> Int)) . chunksOf 1) lns
  return $ map makeCells ints
  where makeCells :: [Int] -> [Cell]
        makeCells i = map (, True) i

loadLines :: String -> IO [String]
loadLines filePath = do
  fileCont <- readFile filePath
  return $ lines fileCont

markAll :: [[Cell]] -> [[Cell]]
markAll grid = transpose (map markCandidates (transpose (map markCandidates grid)))

solve :: [[Cell]] -> Int
solve grid = length lowest + sum (map fst lowest)
  where marked = markAll grid
        lowest = filter snd (concat marked)

