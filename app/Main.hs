{-# LANGUAGE OverloadedStrings   #-}


module Main where

import           System.Environment     (getArgs)
import           Text.Printf            (printf)

import           Streamly
import qualified Streamly.Prelude       as S
import qualified Data.Text              as T

import           Parse                  (getIdx, parseDuration)
import           Twitch                 (getVideoInfo, mkTwitchCfg, VideoInfo(..))
-- import           Data.Map                  (Map)
import           TsIO                   (processM3U8, processTS)
import           Control.Monad.Reader   (runReaderT)


main :: IO ()
main = do
  [vodId, start, end] <- getArgs
  printf "start downloading vod: %s ...\n"  vodId

  let cfg = mkTwitchCfg "https://api.twitch.tv/helix/" "g9r0psjr0nn0a4ypjh62b6p568jhom"

  videoInfo  <- runReaderT (getVideoInfo vodId) cfg
  let vodBaseUrl  = videoinfo_baseUrl videoInfo
      vodUserName = videoinfo_userDisplayName videoInfo

  printf "vod base url => %s\n" vodBaseUrl

  let startTime = T.pack start
      endTime = T.pack end
      -- m3u8file :: T.Text
      m3u8file = "index-dvr.m3u8"
      -- mp4file  :: T.Text
      fileExt = ".mp4"
      fileName  = T.intercalate "_" $ T.pack <$> [ vodId , filter (/= ':') start , filter (/= ':') end ]
      mp4file  = vodUserName <> "_" <> fileName <> fileExt

  processM3U8 vodBaseUrl m3u8file

  m3u8Content <- readFile (T.unpack m3u8file)
  let sIdx = getIdx (parseDuration start) m3u8Content
      eIdx = getIdx (parseDuration end) m3u8Content
  printf "download ts range: %d ~ %d\n" sIdx eIdx

  runStream . serially $ do
    let files = [ sIdx .. eIdx ]
    S.fromFoldableM $ fmap (processTS vodBaseUrl) files

  printf "ts files download completed.\n"
  printf "ffmpeg -i %s -c:v copy -c:a copy -t %s %s\n" m3u8file endTime mp4file
