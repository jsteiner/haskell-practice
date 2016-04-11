import System.IO
import Data.List (isInfixOf, group)

main :: IO ()
main = do
  fromHandle <- openFile "input" ReadMode
  contents   <- hGetContents fromHandle
  putStr . show . length . niceStrings . lines $ contents

niceStrings :: [String] -> [String]
niceStrings = filter niceString

niceString :: String -> Bool
niceString s = and [ threeVowels s
                   , duplicateChain s
                   , not $ hasNaughtyLetters s
                   ]

threeVowels :: String -> Bool
threeVowels = (>2) . length . filter isVowel

isVowel :: Char -> Bool
isVowel c = c `elem` "aeiou"

duplicateChain :: String -> Bool
duplicateChain = any ((>1) . length) . group

hasNaughtyLetters :: String -> Bool
hasNaughtyLetters s = any (`isInfixOf` s) naughtyLetters

naughtyLetters = ["ab", "cd", "pq", "xy"]
