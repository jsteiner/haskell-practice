module Cipher where

import Data.Char

caesar :: Int -> String -> String
caesar offset = fmap $ caesarChar offset

caesarChar :: Int -> Char -> Char
caesarChar offset c
    | isAsciiLower c = shift 'a' offset c
    | isAsciiUpper c = shift 'A' offset c
    | otherwise = c

shift :: Char -> Int -> Char -> Char
shift base offset c =
    chr $ base' + baseZeroOffset
    where c' = ord c
          base' = ord base
          baseZeroOffset = (c' - base' + offset) `mod` 26
