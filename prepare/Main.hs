module Main where

import Data.Char (isAlpha, isAscii)
import Data.List (sortBy)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

main :: IO ()
main = do
  contents <- readFile "data/enwiki-2023-04-13.txt"
  let ls = lines contents
  let entries = [e | Just e <- map parseEntry ls]
  let wordMap = buildMap entries
  let total = fromIntegral (sum (M.elems wordMap)) :: Double
  let entropyMap = M.map (\c -> negate (logBase 2 (fromIntegral c / total))) wordMap
  let outputLines = map (\(w, e) -> w ++ " " ++ show e) (M.toAscList entropyMap)
  writeFile "data/entropy.txt" (unlines outputLines)

parseEntry :: String -> Maybe (String, Int)
parseEntry lines =
  case words lines of
    [w, n] | all isAscii w -> Just (w, read n)
    _ -> Nothing

buildMap :: [(String, Int)] -> M.Map String Int
buildMap entries =
  let expanded = concatMap expand entries
      merged = M.unionsWith (+) expanded
   in M.filter (>= 50) merged

expand :: (String, Int) -> [M.Map String Int]
expand (w, n) =
  let subwords = splitOn "-" w
      cleaned = filter (not . null) (map (filter isAlpha) subwords)
   in map (`M.singleton` n) cleaned