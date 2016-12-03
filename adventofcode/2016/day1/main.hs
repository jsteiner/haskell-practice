#!/usr/bin/env stack
{- stack
  --resolver lts-5.17
  --install-ghc
  runghc
  -- -Wall -Werror
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Text.IO as T
import Data.Attoparsec.Text (Parser, parseOnly, decimal, string, sepBy)
import Control.Applicative ((<|>))

data Movement
    = Movement RelativeDirection Distance

type Distance
    = Int

data RelativeDirection
    = Left'
    | Right'

data Position
    = Position
        { pLocation :: Location
        , pDirection :: CardinalDirection
        }

data Location
    = Location Int Int

data CardinalDirection
    = North
    | East
    | South
    | West
    deriving (Enum)

main :: IO ()
main = do
    contents <- T.readFile "input"
    case parseOnly parseMovements contents of
        Left err -> print err
        Right movements -> print $ totalDistance $ endLocation movements

parseMovements :: Parser [Movement]
parseMovements = parseMovement `sepBy` string ", "

parseMovement :: Parser Movement
parseMovement = Movement <$> parseDirection <*> decimal

parseDirection :: Parser RelativeDirection
parseDirection = (string "L" >> pure Left') <|> (string "R" >> pure Right')

endLocation :: [Movement] -> Location
endLocation = pLocation . endPosition

endPosition :: [Movement] -> Position
endPosition = foldl reposition initialPosition
    where
        initialDirection = North
        initialLocation = Location 0 0
        initialPosition = Position initialLocation initialDirection

totalDistance :: Location -> Int
totalDistance (Location x y) = abs x + abs y

reposition :: Position -> Movement -> Position
reposition Position{..} (Movement direction distance) =
    Position newLocation newDirection
  where
    newDirection = turn direction pDirection
    newLocation = move newDirection pLocation distance

move :: CardinalDirection -> Location -> Distance -> Location
move North (Location x y) distance = Location x (y + distance)
move East (Location x y) distance = Location (x + distance) y
move South (Location x y) distance = Location x (y - distance)
move West (Location x y) distance = Location (x - distance) y

turn :: RelativeDirection -> CardinalDirection -> CardinalDirection
turn Left' = turnLeft
turn Right' = turnRight

turnLeft :: CardinalDirection -> CardinalDirection
turnLeft North = West
turnLeft d = pred d

turnRight :: CardinalDirection -> CardinalDirection
turnRight West = North
turnRight d = succ d
