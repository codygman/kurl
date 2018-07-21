{-# LANGUAGE OverloadedStrings   #-}


module Main where

import           System.Environment     (getArgs)
import           Text.Printf            (printf)

import           Streamly
import qualified Streamly.Prelude       as S
import qualified Data.Text              as T

import           Rename                 (vodRespMapper)
import           Parse                  (getIdx, parseDuration)
import           Twitch                 (makeVodBaseUrl)
import           TsIO                   (processM3U8, processTS)


main :: IO ()
main = do
  [vodId, start, end, output] <- getArgs
  printf "start downloading vod: %s ...\n"  vodId

  vodBaseUrl <- makeVodBaseUrl vodId

  printf "vod base url => %s\n" vodBaseUrl

  let m3u8file = "index-dvr.m3u8"
  processM3U8 vodBaseUrl m3u8file

  m3u8Content <- readFile (T.unpack m3u8file)
  let sIdx = getIdx (parseDuration start) m3u8Content
      eIdx = getIdx (parseDuration end) m3u8Content
  printf "download ts range: %d ~ %d\n" sIdx eIdx

  runStream . serially $ do
    let files = [ sIdx .. eIdx ]
    S.fromFoldableM $ fmap (processTS vodBaseUrl) files

  printf "ts files download completed.\n"
  printf "ffmpeg -i %s -c:v copy -c:a copy -t %s %s\n" m3u8file end output
