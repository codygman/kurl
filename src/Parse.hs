module Parse
  ( filterTime
  , getTime
  , getIdx
  , parseDuration
  ) where

import Data.List
import Data.List.Split

filterTime :: [String] -> [String]
filterTime lines =
  let prefix = "#EXTINF:"
  in filter (isPrefixOf prefix) lines


getTime :: String -> Float
getTime line =
  let prefix = "#EXTINF:"
  in  read . init $ drop (length prefix) line


getIdx :: Float -> String -> Int
getIdx duration m3u8 =
  let durations = fmap getTime $ filterTime $ lines m3u8
      cumulation = scanl1 (+) durations
  in length $ takeWhile (< duration) cumulation


parseDuration :: String -> Float
parseDuration s =
  let [hh,mm,ss] = read <$> wordsBy (== ':') s
  in hh * 3600 + mm * 60 + ss
