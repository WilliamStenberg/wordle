{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Vocabulary where

import System.Random (newStdGen, randomRs)

newtype Vocabulary = Vocabulary [String]

readWordsOfLength :: Int -> IO Vocabulary
readWordsOfLength n =
  Vocabulary
    . filter ((== n) . length)
    . map init
    . lines
    <$> readFile "words.txt"

sample :: Int -> Vocabulary -> IO [String]
sample n (Vocabulary xs) =
  do
    fmap (xs !!)
    . take n
    . System.Random.randomRs (0, length xs - 1)
    <$> System.Random.newStdGen
