{-# LANGUAGE OverloadedStrings   #-}


module Main where

import           System.Environment     (getArgs)
import           Text.Printf            (printf)

import           Streamly
import qualified Streamly.Prelude       as S
import qualified Data.Text              as T

import           Parse                  (getStartIdx, getEndIdx, parseDuration, parseVodUrl, format4file, format4ffmpeg)
import           Twitch                 (getVideoInfo, getChatLogs, TwitchCfg(..), VideoInfo(..))
import           TsIO                   (processM3U8, processTS, writeComments)
import           Control.Monad.Reader   (runReaderT, guard)
import           Data.Maybe
import           Data.Time
import           Options.Applicative
import           Data.String
import           Data.Char


data CmdOpts = CmdOpts
  { vodId  :: String
  , start  :: String
  , end    :: String
  , chat   :: Bool
  }


main :: IO ()
main =  do
  (CmdOpts vod start end chat) <- parseCmdOpts
  let (sutc, eutc) = parseRange start end
  let cfg = TwitchCfg "https://api.twitch.tv/helix/" "g9r0psjr0nn0a4ypjh62b6p568jhom"

  (VideoInfo url user) <- getVodInfo vod cfg

  downloadChat vod user sutc eutc chat cfg

  let m3u8 = "index-dvr.m3u8"
  downloadVod vod url sutc eutc m3u8
  printEncodingCmd vod user sutc eutc m3u8



parseCmdOpts :: IO CmdOpts
parseCmdOpts = execParser $ info
  ( cmd <**> helper ) ( fullDesc <> progDesc "Download twitch vod TARGET" <> header "kurl - a twitch vod downloader" )
  where
    cmd = CmdOpts
      <$> strArgument ( metavar "URL" <> help "twitch vod URL ex) https://www.twitch.tv/videos/012345678" )
      <*> strOption   ( long "start"  <> short 's' <> help "recording start offset" )
      <*> strOption   ( long "end"    <> short 'e' <> help "recording end offset" )
      <*> switch      ( long "chat"   <> short 'c' <> help "download vod chat log" )


parseRange :: String -> String -> (UTCTime, UTCTime)
parseRange s e =
  case ((,) <$> parseDuration s <*> parseDuration e) of
    Just (sutc, eutc) -> (sutc, eutc)
    _ -> error "start end time option error time format should be: <0h0m0s|0h0m|0m0s|0h|0m|0s>"


getVodInfo :: String -> TwitchCfg -> IO VideoInfo
getVodInfo vodId cfg = runReaderT (getVideoInfo vodId) cfg


downloadChat :: String -> T.Text -> UTCTime -> UTCTime -> Bool -> TwitchCfg -> IO ()
downloadChat vod user sutc eutc chat cfg =
    if chat then do
      printf "start downloading all comments of vod: %s ...\n"  vod
      logs <- runReaderT (getChatLogs vod sutc eutc) cfg
      writeComments vod user logs
    else
      return ()


downloadVod :: String -> T.Text -> UTCTime -> UTCTime -> T.Text -> IO ()
downloadVod vod url sutc eutc m3u8 = do
  printf "start downloading ts files of vod: %s ...\n" vod
  printf "vod base url => %s\n" url
  let startTime = format4file sutc
      endTime   = format4file eutc
      fileExt   = "mp4" :: T.Text
  -- Downloading index-dvr.m3u8
  processM3U8 url m3u8
  content <- readFile (T.unpack m3u8)
  -- Calculate proper range
  let s = getStartIdx sutc content
      e = getEndIdx eutc content
  printf "download ts range: %d ~ %d\n" s e
  -- Actual download of ts files
  runStream . serially $ do
    S.fromFoldableM $ fmap (processTS url) [ s .. e ]


printEncodingCmd :: String -> T.Text -> UTCTime -> UTCTime -> T.Text -> IO ()
printEncodingCmd vod user sutc eutc m3u8 = do
  let start = format4file sutc
      end   = format4file eutc
      ext   = "mp4" :: String
      mp4  = T.pack $ printf "%s_%s_%s_%s.%s" user vod start end ext
  printf "ts files download completed.\n"
  printf "ffmpeg -i %s -c:v copy -c:a copy %s\n" m3u8 mp4
