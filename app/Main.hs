module Main where

import System.Random ( newStdGen, randomRs)

main :: IO ()
main = do
  ws <- readWords
  targets <- sample 5 ws
  print $ foldr1 (\x y -> x ++ ", " ++ y) targets

readWords :: IO [String]
readWords = do
  content <- readFile "words.txt"
  return $ filter (\x -> length x == 5) $ map init $ lines content

sample :: Int -> [String] -> IO [String]
sample n xs =
  do
    fmap (xs !!)
    . take n
    . System.Random.randomRs (0, length xs - 1)
    <$> System.Random.newStdGen
