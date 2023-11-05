{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo #-}

module Main where

import Board
import Control.Monad.State
import Data.Function ((&))
import Rainbow
import Robot
import UI
import Vocabulary

main :: IO ()
main = do
  ws <- readWordsOfLength 5
  answer <- sample 1 ws
  s2 <- traverse (play human) [boardFromAnswer t | t <- answer]
  print s2

play :: (a, Guesser a) -> Board -> IO GuessResult
play (a, guesser) board =
  do
    (line, a') <- runStateT (guesser board) a
    case checkGuess board line of
      NextRound newBoard -> play (a', guesser) newBoard
      wonOrLost -> pure wonOrLost

human :: (Int, Guesser Int)
human =
  (0, playInteractive)
 where
  playInteractive board =
    do
      score <- get
      line <-
        lift $ do
          putChunksLn $ showBoard board
          putChunkLn $ "Guess a word:" & fore blue
          getLegalLine
      put (score + 1)
      return line
   where
    getLegalLine =
      do
        guess <- getLine
        case length guess of
          x | x == boardLength board -> return guess
          _ -> putStrLn "Wrong length" >> getLegalLine
