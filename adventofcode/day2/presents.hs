import System.IO

data Package = Package { length :: Int
                       , width :: Int
                       , height :: Int
                       } deriving (Show)

main :: IO ()
main = do
  fromHandle <- openFile "input" ReadMode
  contents <- hGetContents fromHandle
  putStr . show $ foldl (\sum p -> sum + requiredPaper p) 0 (toPackages contents)

requiredPaper :: Package -> Int
requiredPaper (Package { Main.length = l, width = w, height = h }) =
  let side = l * w
      front = w * h
      top = h * l
  in
    (2 * side) + (2 * front) + (2 * top) + (minimum [side, front, top])

toPackages :: String -> [Package]
toPackages string =
  map toPackage $ lines string

toPackage :: String -> Package
toPackage string =
  let
    [l, w, h] = split (== 'x') string
    l' = read l :: Int
    w' = read w :: Int
    h' = read h :: Int
  in
    Package l' w' h'

split :: (Char -> Bool) -> String -> [String]
split p s =
  case dropWhile p s of
    "" -> []
    s' -> w : split p s''
          where (w, s'') = break p s'
