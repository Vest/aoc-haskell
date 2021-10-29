module Day5 where

import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.Digest.Pure.MD5 as MD5
import Data.List (find, stripPrefix)
import qualified Data.Map as M
import Data.Maybe

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
solution input = solution1 input ++ ", " ++ solution2 input

getAdvancedPassword :: String -> String
getAdvancedPassword prefix =
  M.elems
    . fromMaybe M.empty
    . find
      (\m -> M.size m == 8)
    $ scanl
      ( \passMap i ->
          let md5 = getMD5 prefix i
              withoutPrefix = stripPrefix "00000" md5
           in case withoutPrefix of
                Just (pos : ltr : _)
                  | pos `elem` ['0' .. '7'] ->
                    if M.notMember pos passMap
                      then M.insert pos ltr passMap
                      else passMap
                _ -> passMap
      )
      M.empty
      [0 ..]

solution2 :: String -> String
solution2 = getAdvancedPassword . head . lines
