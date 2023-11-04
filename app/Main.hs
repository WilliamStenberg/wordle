{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Foldable (fold)
import Data.Function ((&))
import Data.List (intersperse, partition, sort)
import Data.Text (singleton)
import Rainbow
import System.Random (newStdGen, randomRs)

data GuessResult
  = Lost {answer :: String}
  | NextRound Board
  | Won {nGuesses :: Int}

instance Show GuessResult where
  show (Lost{answer}) = "Lost. The answer was " <> answer
  show (NextRound _) = "NextRound"
  show (Won{nGuesses}) = "Won in " <> show nGuesses <> " guesses"

data Board = Board {rows :: [Row], answer :: String}
showBoard :: Board -> [Chunk]
showBoard (Board{rows}) = fold $ intersperse ["\n" & fore red] (showRow <$> rows)
newtype Row = Row [Tile]

showRow :: Row -> [Chunk]
showRow (Row ts) = fold $ intersperse [", " & fore red] $ traverse showTile ts
data Tile = Tile {index :: Int, character :: Char, color :: Color}
instance Eq Tile where
  (==) (Tile{index = i1, character = c1}) (Tile{index = i2, character = c2}) = i1 == i2 && c1 == c2
instance Ord Tile where
  compare (Tile{index = i1}) (Tile{index = i2}) = compare i1 i2

showTile :: Tile -> [Chunk]
showTile Tile{character, color} = [chunk (singleton character) & showColor color]

data Color = Gray | Yellow | Green
showColor :: Color -> (Chunk -> Chunk)
showColor Gray = fore grey
showColor Yellow = fore yellow
showColor Green = fore green

boardLength :: Board -> Int
boardLength (Board{answer}) = length answer

withGuess :: Board -> String -> Board
withGuess (Board{rows, answer}) guess =
  Board{answer, rows = rows <> [row]}
 where
  row = makeRow guess answer

makeRow :: [Char] -> [Char] -> Row
makeRow guessArr answerArr =
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
   in
    if guess == answer
      then Won{nGuesses}
      else
        if nGuesses == boardLength board
          then Lost{answer}
          else NextRound $ board `withGuess` guess

main :: IO ()
main = do
  ws <- readWordsOfLength 5
  targets <- sample 1 ws
  s2 <- traverse playInteractive [Board{rows = [], answer = t} | t <- targets]
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
