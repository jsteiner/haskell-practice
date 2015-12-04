import System.IO
import Data.List

type House = (Integer, Integer)
type Movement = (Integer, Integer)

main :: IO ()
main =  do
  fromHandle <- openFile "input" ReadMode
  contents   <- hGetContents fromHandle
  putStr . show $ housesHit contents

housesHit :: String -> Int
housesHit input =
  let allHouses = foldl moveHouses initialHouses input
  in
    length $ nub allHouses

initialHouses :: [House]
initialHouses = [(0, 0)]

arrowToMovement :: Char -> Maybe Movement
arrowToMovement a
  | a == '>' = Just (1, 0)
  | a == '<' = Just (-1, 0)
  | a == '^' = Just (0, 1)
  | a == 'v' = Just (0, -1)
  | otherwise = Nothing

moveHouses :: [House] -> Char -> [House]
moveHouses houses arrow =
  case arrowToMovement arrow of
    Just movement -> (nextHouse houses movement) : houses
    Nothing -> houses

nextHouse :: [House] -> Movement -> House
nextHouse ((x, y):_) (dx, dy) = (x + dx, y + dy)
