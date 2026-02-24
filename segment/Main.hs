module Main where

import Data.Array (Array, (!))
import qualified Data.Array as A
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as M

main :: IO ()
main = do
  entropyContents <- readFile "data/entropy.txt"
  let entropyMap = parseEntropyFile entropyContents
  contents <- readFile "data/ishmael.dec"
  let msg = filter (/= '\n') contents
  let result = segment entropyMap msg
  writeFile "data/ishmael.seg" (wrap 60 result)

parseEntropyFile :: String -> M.Map String Double
parseEntropyFile contents =
  M.fromAscList [(w, read e) | [w, e] <- map words (lines contents)]

wrap :: Int -> String -> String
wrap n = unlines . map unwords . go 0 [] . words
  where
    go _ line [] = [reverse line]
    go len line (w : ws)
      | len == 0 = go (length w) [w] ws
      | len + 1 + length w > n = reverse line : go (length w) [w] ws
      | otherwise = go (len + 1 + length w) (w : line) ws

segment dict msg = unwords (traceBack 0)
  where
    n = length msg

    prefixWords :: Int -> [(String, Double)]
    prefixWords i =
      let suffix = drop i msg
          (smaller, exact, _) = M.splitLookup suffix dict
          exactMatches = case exact of
            Just e -> [(suffix, e)]
            Nothing -> []
          prefixMatches = [(w, e) | (w, e) <- M.toAscList smaller, w `isPrefixOf` suffix]
       in exactMatches ++ prefixMatches

    assocs :: [(Int, (Double, String))]
    assocs =
      [ (i, best)
        | i <- [0 .. n - 1],
          let candidates =
                [ (e + entropyArr ! (i + length w), w)
                  | (w, e) <- prefixWords i
                ],
          let best = case candidates of
                [] -> (1 / 0, "") -- infinity if no word found
                _ -> minimum candidates
      ]

    entropyArr :: Array Int Double
    entropyArr =
      A.array (0, n) $
        [(i, fst v) | (i, v) <- assocs] ++ [(n, 0.0)]

    wordArr :: Array Int String
    wordArr =
      A.array
        (0, n - 1)
        [(i, snd v) | (i, v) <- assocs]

    traceBack :: Int -> [String]
    traceBack i
      | i >= n = []
      | otherwise =
          let w = wordArr ! i
           in if null w
                then ("<UNKNOWN:" ++ [msg !! i] ++ ">") : traceBack (i + 1)
                else w : traceBack (i + length w)