module Main where

main :: IO ()
main = do
  input <- parse <$> readFile "inputs/day-1.txt"

  putStrLn "Part 1:"
  print $! countIncrements input

countIncrements :: [Int] -> Int
countIncrements (x : y : xs)
  | y > x     = countIncrements (y : xs) + 1
  | otherwise = countIncrements (y : xs)
countIncrements _ = 0

parse :: String -> [Int]
parse = map read . lines
