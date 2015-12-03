import System.IO

main :: IO ()
main =  do
  fromHandle <- openFile "input" ReadMode
  contents   <- hGetContents fromHandle
  putStr . show $ finalFloor contents

finalFloor :: String -> Int
finalFloor input =
  foldl moveFloor 0 input

moveFloor :: Int -> Char -> Int
moveFloor acc c
 | c == '(' = acc + 1
 | c == ')' = acc - 1
 | otherwise = acc
