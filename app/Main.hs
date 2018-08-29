{-# LANGUAGE OverloadedStrings   #-}


module Main where


import           Data.Maybe                  (fromJust, fromMaybe)
import           Data.Time                   (NominalDiffTime, getCurrentTime)
import           Data.Text                   (Text, pack, unpack, intercalate)
import           Options.Applicative
import           System.Info                 (os)
import           Text.Printf                 (printf)
import           Streamly                    (runStream, serially)
import           Streamly.Prelude            (fromFoldableM)

import           M3u8                        (getStartIdx, getEndIdx,)
import           TimeFormat                  (format4file, formatUtc, format4ffmpeg, makeOffset)
import           TsIO                        (processM3U8, processTS, writeComments)
import           Twitch                      (getLive, getArchive, getChatLogs, VideoInfo(..))


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
  cmdOpts <- parseCmdOpts

  let target = parseVodUrl (vodId cmdOpts)

  if not (live cmdOpts) then do
    (VideoInfo fullUrl user duration) <- getArchive (quality cmdOpts) target
    let defaultStart        = "00:00:00"
        startNominalDiff    = makeOffset $ fromMaybe defaultStart (start cmdOpts)
        endNominalDiff      = makeOffset $ fromMaybe (unpack . fromJust $ duration) (end cmdOpts)
        durationNominalDiff = endNominalDiff - startNominalDiff
    printEncodingCmdArchive target user startNominalDiff durationNominalDiff endNominalDiff fullUrl

    if (chat cmdOpts) then do
      downloadChat target user startNominalDiff endNominalDiff
      else return ()

    if (ts cmdOpts) then do
      let localIndexDvrM3u8 = "index-dvr.m3u8"
      downloadVod target fullUrl startNominalDiff endNominalDiff localIndexDvrM3u8
      else return ()

  else do
    (VideoInfo fullUrl user _) <- getLive (quality cmdOpts) (vodId cmdOpts)
    printEncodingCmdLive user fullUrl
    return ()


parseVodUrl :: String -> String
parseVodUrl = reverse . takeWhile (/= '/') . reverse


parseCmdOpts :: IO CmdOpts
parseCmdOpts = execParser $ info
  ( cmd <**> helper ) ( fullDesc <> progDesc ("Download twitch TARGET. TARGET is <vod url> or <channel name>.")
                                 <> header "kurl - a twitch vod downloader" )
  where
    cmd = CmdOpts
      <$> strArgument         ( metavar "TARGET"             <> help targetHelpMsg                     )
      <*> strOption           ( long "qaulity"  <> short 'q' <> help qualityHelpMsg <> value "chunked" )
      <*> switch              ( long "live"     <> short 'l' <> help liveHelpMsg                       )
      <*> switch              ( long "ts"       <> short 't' <> help tsHelpMsg                         )
      <*> optional (strOption ( long "start"    <> short 's' <> help startHelpMsg                      ) )
      <*> optional (strOption ( long "end"      <> short 'e' <> help endHelpMsg                        ) )
      <*> switch              ( long "chat"     <> short 'c' <> help chatHelpMsg                       )
    targetHelpMsg  = "Download targert ex) 123456789 or playhearthstone or https://www.twitch.tv/videos/123456789."
                     <> " -- When downloading live type stream, TARGET must be <channel name>"
                     <> " and downloading archive type stream, TARGET must be <vod url>"
    qualityHelpMsg = "set stream quality. default is chunked."
                     <> " chunked is source quality. ex) chunked, 720p60, 480p30"
    liveHelpMsg    = "set type stream. live or archive. default is archive type"
    tsHelpMsg      = "download ts files of the vod. Supported on only archive type stream."
    startHelpMsg   = "recording start offset"
    endHelpMsg     = "recording end offset"
    chatHelpMsg    = "download vod chat log. Supported on only archive type stream."


downloadChat :: String -> Text -> NominalDiffTime -> NominalDiffTime -> IO ()
downloadChat target user startNominalDiff endNominalDiff = do
  printf "start downloading all comments of vod: %s ...\n"  target
  logs <- getChatLogs target startNominalDiff endNominalDiff
  writeComments target user logs


downloadVod :: String -> Text -> NominalDiffTime -> NominalDiffTime -> Text -> IO ()
downloadVod target url startNominalDiff endNominalDiff m3u8 = do
  printf "start downloading ts files of vod: %s ...\n" target
  printf "vod base url => %s\n" url
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
printEncodingCmdArchive target user s d e m3u8 = do
  let offset    = format4ffmpeg s
      duration  = format4ffmpeg d
      ext       = "mp4" :: String
      mp4       = pack $ printf "%s_%s_%s_%s.%s" user target (format4file s) (format4file e) ext
      formatStr = unpack $ intercalate delim ["ffmpeg", "-ss %s", "-i %s", "-to %s", "-c:v copy","-c:a copy", "%s\n"]
  printf formatStr offset m3u8 duration mp4


printEncodingCmdLive :: Text ->  Text -> IO ()
printEncodingCmdLive channelId m3u8 = do
  cutc <- getCurrentTime
  let ext       = "mp4" :: String
      mp4       = pack $ printf "%s_live_%s.%s" channelId (formatUtc cutc) ext
      formatStr = unpack $ intercalate delim ["ffmpeg", "-i %s", "-c:v copy","-c:a copy", "%s\n"]
  printf formatStr m3u8 mp4


delim :: Text
delim  = if os == "mingw32" || os == "mingw64" then " ^\n" else " \\\n"
