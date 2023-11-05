{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Board
import Data.Function ((&))
import Rainbow
import System.Random (newStdGen, randomRs)
import UI

main :: IO ()
main = do
  ws <- readWordsOfLength 5
  answer <- sample 1 ws
  s2 <- traverse playInteractive [boardFromAnswer t | t <- answer]
  print s2

playInteractive :: Board -> IO GuessResult
playInteractive board =
  do
    putChunksLn $ showBoard board
    putChunkLn $ "Guess a word:" & fore blue
    line <- getLegalLine
    case checkGuess board line of
      NextRound newBoard -> playInteractive newBoard
      wonOrLost -> pure wonOrLost
 where
  getLegalLine =
    do
      guess <- getLine
      -- If length is n
      case length guess of
        x | x == boardLength board -> return guess
        _ -> putStrLn "Wrong length" >> getLegalLine

readWordsOfLength :: Int -> IO [String]
readWordsOfLength n =
  filter ((== n) . length)
    . map init
    . lines
    <$> readFile "words.txt"

sample :: Int -> [String] -> IO [String]
sample n xs =
  do
    fmap (xs !!)
    . take n
    . System.Random.randomRs (0, length xs - 1)
    <$> System.Random.newStdGen
