{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Robot where

import Board
import Control.Monad.State
import Vocabulary

data Robot = Robot {allWords :: Vocabulary, currentSet :: [String], board :: Board}

type Player a = StateT a IO
type Guesser a = Board -> Player a String
