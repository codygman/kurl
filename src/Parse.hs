{-# LANGUAGE RankNTypes #-}


module Parse
  ( filterTime
  , getTime
  , getStartIdx
  , getEndIdx
  , parseDuration
  , parseVodUrl
  , format4file
  , format4ffmpeg
  ) where


import Data.List
import Data.List.Split
import Data.Time
import Data.Maybe
import Data.Time.Format
import Control.Applicative


filterTime :: [String] -> [String]
filterTime lines =
  let prefix = "#EXTINF:"
  in filter (isPrefixOf prefix) lines


getTime :: String -> NominalDiffTime
getTime line =
  case parseTimeM True defaultTimeLocale "#EXTINF:%-S%-Q," line :: Maybe UTCTime of
    Just x  -> toEnum . fromEnum . utctDayTime $ x
    Nothing -> error "EXFINF parse error"


getStartIdx :: UTCTime -> String -> Int
getStartIdx duration m3u8 = getIdx (>=) duration m3u8


getEndIdx :: UTCTime -> String -> Int
getEndIdx duration m3u8 = getIdx (>) duration m3u8


getIdx :: (forall a. Ord a => a -> a -> Bool) -> UTCTime -> String -> Int
getIdx pred duration m3u8 =
  let nominalDuration = toEnum . fromEnum . utctDayTime $ duration
      durations = fmap getTime $ filterTime $ lines m3u8
      cumulation = scanl1 (+) durations
  in  length $ takeWhile (pred nominalDuration)cumulation


parseDuration :: Maybe String -> Maybe UTCTime
parseDuration mx = case mx of
  Nothing -> Nothing
  Just x ->  parseRangeTime "%-Hh%-Mm%-Ss" x
              <|> parseRangeTime "%-Mm%-Ss" x
              <|> parseRangeTime "%-Hh%-Mm" x
              <|> parseRangeTime "%-Hh" x
              <|> parseRangeTime "%-Mm" x
              <|> parseRangeTime "%-Ss" x
  where
    parseRangeTime timeFormat x = parseTimeM True defaultTimeLocale timeFormat x


format4file :: FormatTime t => t -> String
format4file = formatTime defaultTimeLocale "%Hh%Mm%Ss"


format4ffmpeg :: FormatTime t => t -> String
format4ffmpeg = formatTime defaultTimeLocale "%H:%M:%S"


parseVodUrl :: String -> String
parseVodUrl = reverse . takeWhile (/= '/') . reverse

