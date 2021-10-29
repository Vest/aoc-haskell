module Day5 where

import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.Digest.Pure.MD5 as MD5
import Data.List (stripPrefix)

getMD5 :: String -> Int -> String
getMD5 input num =
  let guess = BLU.fromString (input ++ show num)
   in show . MD5.md5 $ guess

getPassword :: String -> String
getPassword prefix =
  head
    . filter
      (\pass -> length pass == 8)
    $ scanl
      ( \pass i ->
          let md5 = getMD5 prefix i
              withoutPrefix = stripPrefix "00000" md5
           in case withoutPrefix of
                Just p -> pass ++ [head p]
                Nothing -> pass
      )
      []
      [0 ..]

solution1 :: String -> String
solution1 = getPassword . head . lines

solution :: String -> String
solution = solution1
