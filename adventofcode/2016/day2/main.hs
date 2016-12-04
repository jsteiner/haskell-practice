#!/usr/bin/env stack
{- stack
  --resolver lts-5.17
  --install-ghc
  runghc
  -- -Wall -Werror
-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

data Movement = Movement Char
data Point = Point Int Int
type Button = Char
type Code = [Button]

main :: IO ()
main = putStrLn . decipherCode =<< readFile "input"

decipherCode :: String -> Code
decipherCode = toButtons . toMovements

toMovements :: String -> [[Movement]]
toMovements = map (map Movement) . lines

toButtons :: [[Movement]] -> Code
toButtons = map toButton . findPoints

findPoints :: [[Movement]] -> [Point]
findPoints = foldl findPoints' []
  where
    findPoints' :: [Point] -> [Movement] -> [Point]
    findPoints' [] movements = [findPoint initialPoint movements]
    findPoints' ps movements = ps ++ [findPoint (last ps) movements]

findPoint :: Point -> [Movement] -> Point
findPoint = foldl move

initialPoint :: Point
initialPoint = Point 1 1

move :: Point -> Movement -> Point
move (Point x y) (Movement 'U') = Point x (max 0 (y - 1))
move (Point x y) (Movement 'D') = Point x (min 2 (y + 1))
move (Point x y) (Movement 'L') = Point (max 0 (x - 1)) y
move (Point x y) (Movement 'R') = Point (min 2 (x + 1)) y
move _ _ = error "Invalid movement"

toButton :: Point -> Button
toButton (Point x y) = (numberPad !! y) !! x

numberPad :: [[Button]]
numberPad =
    ["123"
    ,"456"
    ,"789"
    ]
