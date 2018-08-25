{-# LANGUAGE OverloadedStrings   #-}


module Main where


import           Control.Monad.Reader        (runReaderT)
import           Data.Maybe                  (fromJust, fromMaybe)
import           Data.Time                   (NominalDiffTime, getCurrentTime)
import           Data.Text                   (Text, pack, unpack, intercalate)
import           Options.Applicative
import           System.Environment          (getArgs)
import           System.Info                 (os)
import           Text.Printf                 (printf)
import           Streamly                    (runStream, serially)
import           Streamly.Prelude            (fromFoldableM)

import           Parse                       (getStartIdx, getEndIdx, parseVodUrl, format4file, formatUtc, format4ffmpeg, makeOffset)
import           Twitch                      (getLive, getArchive, getChatLogs, TwitchCfg(..), VideoInfo(..), StreamType(..))
import           TsIO                        (processM3U8, processTS, writeComments)


data CmdOpts = CmdOpts
  { vodId   :: String
  , quality :: String
  , live    :: Bool
  , ts      :: Bool
  , start   :: Maybe String
  , end     :: Maybe String
  , chat    :: Bool
  }


main :: IO ()
main =  do
  let cfg = TwitchCfg "https://api.twitch.tv/v5/videos" -- v5 api endpoint
                      "https://api.twitch.tv/helix"     -- new api endpoint
                      "g9r0psjr0nn0a4ypjh62b6p568jhom"  -- cliendId
                      "comments?content_offset_seconds" -- chat log path

  (CmdOpts vod quality live ts start end chat) <- parseCmdOpts

  let vodId = parseVodUrl vod

  if not live then do
    (VideoInfo fullUrl user duration) <- runReaderT (getArchive quality vodId) cfg
    let defaultStart        = "00:00:00"
        startNominalDiff    = makeOffset (fromMaybe defaultStart start)
        durationNominalDiff = makeOffset (unpack . fromJust $ duration)
        endNominalDiff      = toEnum $ fromEnum startNominalDiff + fromEnum durationNominalDiff
    printEncodingCmdArchive vodId user startNominalDiff durationNominalDiff endNominalDiff fullUrl

    if chat then do
      downloadChat vodId user startNominalDiff endNominalDiff chat cfg
      else return ()

    if ts then do
      let localIndexDvrM3u8 = "index-dvr.m3u8"
      downloadVod vodId fullUrl startNominalDiff endNominalDiff localIndexDvrM3u8
      else return ()

  else do
    (VideoInfo fullUrl user _) <- runReaderT (getLive quality vodId) cfg
    printEncodingCmdLive user fullUrl
    return ()


parseCmdOpts :: IO CmdOpts
parseCmdOpts = execParser $ info
  ( cmd <**> helper ) ( fullDesc <> progDesc ("Download twitch TARGET. TARGET is <vod url> or <channel name>."
                                             )
                                 <> header "kurl - a twitch vod downloader" )
  where
    cmd = CmdOpts
      <$> strArgument ( metavar "TARGET" <> help ("Download targert ex) 123456789 or playhearthstone or https://www.twitch.tv/videos/123456789."
                                                 <> " -- When downloading live type stream, TARGET must be <channel name>"
                                                 <> " and downloading archive type stream, TARGET must be <vod url>"))
      <*> strOption             ( long "qaulity"  <> short 'q' <> value "chunked" <> help ("set stream quality. default is chunked."
                                                                                          <> " chunked is source quality. ex) chunked, 720p60, 480p30"))
      <*> switch                ( long "live"     <> short 'l' <> help "set type stream. live or archive. default is archive type" )
      <*> switch                ( long "ts"       <> short 't' <> help "download ts files of the vod. Supported on only archive type stream." )
      <*> optional (strOption   ( long "start"    <> short 's' <> help "recording start offset" ))
      <*> optional (strOption   ( long "end"      <> short 'e' <> help "recording end offset" ))
      <*> switch                ( long "chat"     <> short 'c' <> help "download vod chat log. Supported on only archive type stream." )



downloadChat :: String -> Text -> NominalDiffTime -> NominalDiffTime -> Bool -> TwitchCfg -> IO ()
downloadChat vodId user startNominalDiff endNominalDiff chat cfg =
    if chat then do
      printf "start downloading all comments of vod: %s ...\n"  vodId
      logs <- runReaderT (getChatLogs vodId startNominalDiff endNominalDiff) cfg
      writeComments vodId user logs
    else
      return ()


downloadVod :: String -> Text -> NominalDiffTime -> NominalDiffTime -> Text -> IO ()
downloadVod vodId url startNominalDiff endNominalDiff m3u8 = do
  printf "start downloading ts files of vod: %s ...\n" vodId
  printf "vod base url => %s\n" url
  let startTime = format4file startNominalDiff
      endTime   = format4file endNominalDiff
      fileExt   = "mp4" :: Text
  -- Downloading index-dvr.m3u8
  processM3U8 url m3u8
  content <- readFile (unpack m3u8)
  -- Calculate proper range
  let s = getStartIdx startNominalDiff content
      e = getEndIdx endNominalDiff content
  printf "download ts range: %d ~ %d\n" s e
  -- Actual download of ts files
  runStream . serially $ do
    fromFoldableM $ fmap (processTS url) [ s .. e ]


printEncodingCmdArchive :: String -> Text -> NominalDiffTime -> NominalDiffTime -> NominalDiffTime -> Text -> IO ()
printEncodingCmdArchive vodId user s d e m3u8 = do
  let start     = format4ffmpeg s
      duration  = format4ffmpeg d
      ext       = "mp4" :: String
      mp4       = pack $ printf "%s_%s_%s_%s.%s" user vodId (format4file s) (format4file e) ext
      formatStr = unpack $ intercalate delim ["ffmpeg", "-ss %s", "-t %s", "-i %s", "-c:v copy","-c:a copy", "%s\n"]
  printf formatStr start duration m3u8 mp4


printEncodingCmdLive :: Text ->  Text -> IO ()
printEncodingCmdLive channelId m3u8 = do
  cutc <- getCurrentTime
  let ext       = "mp4" :: String
      mp4       = pack $ printf "%s_live_%s.%s" channelId (formatUtc cutc) ext
      formatStr = unpack $ intercalate delim ["ffmpeg", "-i %s", "-c:v copy","-c:a copy", "%s\n"]
  printf formatStr m3u8 mp4


delim :: Text
delim  = if os == "mingw32" || os == "mingw64" then " ^\n" else " \\\n"
