{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Board where

import Data.List (partition, sort)

data GuessResult
  = Lost Answer
  | NextRound Board
  | Won {nGuesses :: Int}

instance Show GuessResult where
  show (Lost (Answer a)) = "Lost. The answer was " <> a
  show (NextRound _) = "NextRound"
  show (Won{nGuesses}) = "Won in " <> show nGuesses <> " guesses"
newtype Answer = Answer String

data Board = Board {rows :: [Row], answer :: Answer}
newtype Row = Row [Tile]

data Tile = Tile {index :: Int, character :: Char, color :: Color}
instance Eq Tile where
  (==) (Tile{index = i1, character = c1}) (Tile{index = i2, character = c2}) = i1 == i2 && c1 == c2
instance Ord Tile where
  compare (Tile{index = i1}) (Tile{index = i2}) = compare i1 i2

data Color = Gray | Yellow | Green

boardLength :: Board -> Int
boardLength (Board{answer = Answer a}) = length a

boardFromAnswer :: String -> Board
boardFromAnswer ans =
  Board{answer = Answer ans, rows = []}

withGuess :: Board -> String -> Board
withGuess (Board{rows, answer}) guess =
  Board{answer, rows = rows <> [row]}
 where
  row = makeRow guess answer

makeRow :: [Char] -> Answer -> Row
makeRow guessArr (Answer answerArr) =
  let
    index = zip [(1 :: Int) ..]
    (gs1, as1, ts1) = withCorrect (index guessArr) (index answerArr)
    ts2 = withMisplaced gs1 as1
   in
    Row $ sort (ts1 <> ts2)

type C = (Int, Char)
type R = ([C], [C], [Tile])

to :: Color -> C -> Tile
to color (i, c) = Tile{index = i, character = c, color}

withCorrect :: [C] -> [C] -> R
withCorrect =
  go ([], [], [])
 where
  go acc (g : gs) (a : as) =
    let
      (accGs, accAs, accTs) = acc
     in
      if g == a
        then go (accGs, accAs, accTs <> [to Green g]) gs as
        else go (accGs <> [g], accAs <> [a], accTs) gs as
  go acc _ _ = acc

withMisplaced :: [C] -> [C] -> [Tile]
withMisplaced =
  go
 where
  go :: [C] -> [C] -> [Tile]
  go (g : gs) as =
    case find g of
      Nothing -> [to Gray g] <> go gs as
      Just otherAs -> [to Yellow g] <> go gs otherAs
   where
    find :: C -> Maybe [C]
    find (_, c) =
      case partition (\(_, c') -> c == c') as of
        ([], _) -> Nothing
        (_, otherAs) -> Just otherAs
  go _ _ = []

checkGuess :: Board -> String -> GuessResult
checkGuess board@(Board{answer, rows}) guess =
  let
    nGuesses = length rows + 1
    Answer a = answer
   in
    if guess == a
      then Won{nGuesses}
      else
        if nGuesses == boardLength board
          then Lost answer
          else NextRound $ board `withGuess` guess
