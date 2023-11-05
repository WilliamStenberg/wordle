{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module UI where

import Board
import Data.Foldable (fold)
import Data.Function ((&))
import Data.List (intersperse)
import Data.Text (singleton)
import Rainbow

showBoard :: Board -> [Chunk]
showBoard (Board{rows}) = fold $ intersperse ["\n" & fore red] (showRow <$> rows)

showRow :: Row -> [Chunk]
showRow (Row ts) = fold $ intersperse [", " & fore red] $ traverse showTile ts

showTile :: Tile -> [Chunk]
showTile Tile{character, color} = [chunk (singleton character) & showColor color]

showColor :: Color -> (Chunk -> Chunk)
showColor Gray = fore grey
showColor Yellow = fore yellow
showColor Green = fore green
