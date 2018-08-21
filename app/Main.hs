{-# LANGUAGE OverloadedStrings   #-}


module Main where

import           System.Environment     (getArgs)
import           System.Info            (os)
import           Text.Printf            (printf)

import           Streamly
import qualified Streamly.Prelude       as S
import qualified Data.Text              as T

import           Parse                  (getStartIdx, getEndIdx, parseDuration, parseVodUrl, format4file, format4ffmpeg)
import           Twitch                 (getVideoInfo, getChatLogs, TwitchCfg(..), VideoInfo(..))
import           TsIO                   (processM3U8, processTS, writeComments, makeDnUrl)
import           Control.Monad.Reader   (runReaderT, guard)
import           Data.Maybe
import           Data.Time
import           Options.Applicative
import           Data.String
import           Data.Char


data CmdOpts = CmdOpts
  { vodId   :: String
  , start   :: Maybe String
  , end     :: Maybe String
  , chat    :: Bool
  , ts      :: Bool
  }


main :: IO ()
main =  do
  let cfg = TwitchCfg "https://api.twitch.tv/v5/videos" -- v5 api endpoint
                      "https://api.twitch.tv/helix"     -- new api endpoint
                      "g9r0psjr0nn0a4ypjh62b6p568jhom"  -- cliendId
                      "comments?content_offset_seconds" -- chat log path

  (CmdOpts vod start end chat ts) <- parseCmdOpts

  let vodId = parseVodUrl vod

  (VideoInfo url user duration) <- getVodInfo vodId cfg

  let defaultStart    = "0s"
      defaultDuration = duration
      (sutc, eutc)    = parseRange start end defaultStart defaultDuration

  if chat
    then downloadChat vodId user sutc eutc chat cfg
    else return ()

  let m3u8 = "index-dvr.m3u8"
  let fullm3u8 = makeDnUrl url m3u8

  printEncodingCmd vodId user sutc eutc fullm3u8

  if ts
    then do
      downloadVod vodId url sutc eutc m3u8
    else return ()


parseCmdOpts :: IO CmdOpts
parseCmdOpts = execParser $ info
  ( cmd <**> helper ) ( fullDesc <> progDesc "Download twitch vod TARGET" <> header "kurl - a twitch vod downloader" )
  where
    cmd = CmdOpts
      <$> strArgument ( metavar "URL"   <> help "twitch vod URL ex) https://www.twitch.tv/videos/012345678" )
      <*> optional (strOption   ( long "start"    <> short 's' <> help "recording start offset" ))
      <*> optional (strOption   ( long "end"      <> short 'e' <> help "recording end offset" ))
      <*> switch                ( long "chat"     <> short 'c' <> help "download vod chat log" )
      <*> switch                ( long "ts"       <> short 't' <> help "download ts files of the vod" )


parseRange :: Maybe String -> Maybe String -> T.Text -> T.Text -> (UTCTime, UTCTime)
parseRange s e defaultS defaultD =
  let sutc = parseDuration s <|> parseDuration (Just . T.unpack $ defaultS)
      eutc = parseDuration e <|> parseDuration (Just . T.unpack $ defaultD)
  in (fromJust sutc, fromJust eutc)


getVodInfo :: String -> TwitchCfg -> IO VideoInfo
getVodInfo vodId cfg = runReaderT (getVideoInfo vodId) cfg


downloadChat :: String -> T.Text -> UTCTime -> UTCTime -> Bool -> TwitchCfg -> IO ()
downloadChat vodId user sutc eutc chat cfg =
    if chat then do
      printf "start downloading all comments of vod: %s ...\n"  vodId
      logs <- runReaderT (getChatLogs vodId sutc eutc) cfg
      writeComments vodId user logs
    else
      return ()


downloadVod :: String -> T.Text -> UTCTime -> UTCTime -> T.Text -> IO ()
downloadVod vodId url sutc eutc m3u8 = do
  printf "start downloading ts files of vod: %s ...\n" vodId
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
printEncodingCmd vodId user sutc eutc m3u8 = do
  let duration  = format4ffmpeg $ addUTCTime (diffUTCTime eutc sutc) (UTCTime (toEnum 0) (toEnum 0))
      ext       = "mp4" :: String
      mp4       = T.pack $ printf "%s_%s_%s_%s.%s" user vodId (format4file sutc) (format4file eutc) ext
      delim     = if os == "mingw32" || os == "mingw64" then " ^\n" else " \\\n"
      formatStr = T.unpack $ T.intercalate delim ["ffmpeg", "-ss %s", "-t %s", "-i %s", "-c:v copy","-c:a copy", "%s\n"]
  printf "Encoding command with ffmpeg.\n"
  printf $ (format4ffmpeg sutc)
  printf formatStr (format4ffmpeg sutc) duration m3u8 mp4
