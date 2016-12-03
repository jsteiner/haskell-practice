import System.IO
import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.MD5 (md5)

main :: IO ()
main =  do
  putStr . show $ lowestAdventCoin "yzbqklnj"

lowestAdventCoin :: String -> Int
lowestAdventCoin secretKey = until (hashHasLeadingZeros secretKey) (+1) 1

hashHasLeadingZeros :: String -> Int -> Bool
hashHasLeadingZeros secretKey i =
  let postfix = secretKey ++ show i
      first5 = take 5 $ hash postfix
  in
    first5 == "00000"

hash :: String -> String
hash = show . md5 . pack
