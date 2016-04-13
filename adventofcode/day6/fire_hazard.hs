module Main where

import Control.Monad (void)
import Text.ParserCombinators.Parsec
import Data.List (foldl')
import Data.Text (Text)
import Data.Array.Unboxed (UArray, (//), (!))
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Array.Unboxed as A

type Light = Bool
type Point = (Int, Int)
type Range = (Point, Point)
type Grid = UArray (Int, Int) Light
data Instruction
    = Toggle { _range :: Range }
    | TurnOn { _range :: Range }
    | TurnOff { _range :: Range }
    deriving (Show)

main :: IO ()
main = do
    contents <- T.readFile "input"
    let instructions = parseInstructions contents
    let finalGrid = followInstructions initialGrid instructions

    print $ numOn finalGrid

parseInstructions :: Text -> [Instruction]
parseInstructions = fmap parseLine . T.lines

parseLine :: Text -> Instruction
parseLine = getRight . parse parseInstruction "content" . T.unpack

getRight :: Either ParseError Instruction -> Instruction
getRight (Right i) = i
getRight _ = error "Failed to parse"

parseInstruction :: Parser Instruction
parseInstruction = do
    t <- parseToggle
        <|> parseTurnOn
        <|> parseTurnOff
    void space
    p1 <- parsePoint
    void $ string " through "
    p2 <- parsePoint

    return $ t (p1, p2)

parseToggle :: Parser (Range -> Instruction)
parseToggle = try $ string "toggle" >> pure Toggle

parseTurnOn :: Parser (Range -> Instruction)
parseTurnOn = try $ string "turn on" >> pure TurnOn

parseTurnOff :: Parser (Range -> Instruction)
parseTurnOff = try $ string "turn off" >> pure TurnOff

parsePoint :: Parser Point
parsePoint = do
    x <- many1 digit
    void $ string ","
    y <- many1 digit

    pure (read x, read y)

followInstructions :: Grid -> [Instruction] -> Grid
followInstructions = foldl' followInstruction

followInstruction :: Grid -> Instruction -> Grid
followInstruction g i =
    g // [ (pt, exec i (g ! pt)) | pt <- A.range $ _range i ]

exec :: Instruction -> Light -> Light
exec (Toggle _) l = not l
exec (TurnOn _) _ = True
exec (TurnOff _) _ = False

numOn :: Grid -> Int
numOn = length . filter (== True) . A.elems

initialGrid :: Grid
initialGrid = A.listArray ((0,0), (size,size)) $ repeat False

size :: Int
size = 999
