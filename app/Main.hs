{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase   #-}


module Main where


import           Control.Monad               (when)
import           Data.Maybe                  (fromJust, fromMaybe)
import           Data.Time                   (NominalDiffTime, getCurrentTime)
import           Data.Text                   (Text, pack, unpack, intercalate)
import qualified Dhall                    as Dh
import           Options.Applicative
import           System.Info                 (os)
import           Text.Printf                 (printf)
import           Streamly                    (runStream, serially)
import           Streamly.Prelude            (fromFoldableM)

import           M3u8                        (getStartIdx, getEndIdx,)
import           TimeFormat                  (format4file, formatUtc, format4ffmpeg, makeOffset)
import           TsIO                        (processM3U8, processTS, writeComments)
import           Twitch                      (getLive, getArchive, getChatLogs, VideoInfo(..))
import           Conf                        (KurlConf(..))

data CmdOpts = CmdOpts
  { vodId   :: String
  , live    :: Maybe LiveOrArchive
  , bare    :: Maybe BareOrFull
  , quality :: String
  , ts      :: Bool
  , start   :: Maybe String
  , end     :: Maybe String
  , chat    :: Bool
  }


data LiveOrArchive = Live | Archive
data BareOrFull    = Bare | Full


main :: IO ()
main = do
  kurlConf <- Dh.input Dh.auto "./kurl.config" :: IO KurlConf

  cmdOpts <- parseCmdOpts

  let liveness = maybe (kurlConfLive kurlConf)
                       (\case Live -> True; Archive -> False)
                       (live cmdOpts)
      bareness = maybe (kurlConfBare kurlConf)
                       (\case Bare -> True; Full -> False)
                       (bare cmdOpts)

  let target = parseVodUrl (vodId cmdOpts)

  if not liveness then do
    (VideoInfo fullUrl user duration) <- getArchive kurlConf (quality cmdOpts) target
    let defaultStart        = "00:00:00"
        startNominalDiff    = makeOffset $ fromMaybe defaultStart (start cmdOpts)
        endNominalDiff      = makeOffset $ fromMaybe (unpack . fromJust $ duration) (end cmdOpts)
        durationNominalDiff = endNominalDiff - startNominalDiff

    if bareness
      then
        printf "%s" fullUrl
      else do
        printEncodingCmdArchive target user startNominalDiff durationNominalDiff endNominalDiff fullUrl

        when (chat cmdOpts) $ do
         printf "Start downloading chat...\n"
         downloadChat kurlConf target user startNominalDiff endNominalDiff

        when (ts cmdOpts) $ do
          -- TODO: this is not proper file name for index-dvr.m3u8
          --       this must be extracted from full url.
          let localIndexDvrM3u8 = "index-dvr.m3u8"
          downloadVod target fullUrl startNominalDiff endNominalDiff localIndexDvrM3u8

  else do
    VideoInfo fullUrl user _ <- getLive kurlConf (quality cmdOpts) (vodId cmdOpts)
    if bareness
      then printf "%s" fullUrl
    else
      printEncodingCmdLive user fullUrl


parseVodUrl :: String -> String
parseVodUrl = reverse . takeWhile (/= '/') . reverse


parseCmdOpts :: IO CmdOpts
parseCmdOpts = execParser $ info
  ( cmd <**> helper ) ( fullDesc <> progDesc "Download twitch TARGET. TARGET is <vod url> or <channel name>."
                                 <> header "kurl - a twitch vod downloader" )
  where
    cmd = CmdOpts
      <$> strArgument         ( metavar "TARGET"             <> help targetHelpMsg                     )
      <*> optional (   flag' Archive  ( long "arch"     <> short 'a' <> help archHelpMsg               )
                   <|> flag' Live     ( long "live"     <> short 'l' <> help liveHelpMsg               ))
      <*> optional (   flag' Full     ( long "full"     <> short 'f' <> help fullHelpMsg               )
                   <|> flag' Bare     ( long "bare"     <> short 'b' <> help bareHelpMsg               ))
      <*> strOption           ( long "quality"  <> short 'q' <> help qualityHelpMsg <> value "chunked" )
      <*> switch              ( long "ts"       <> short 't' <> help tsHelpMsg                         )
      <*> optional (strOption ( long "start"    <> short 's' <> help startHelpMsg                      ) )
      <*> optional (strOption ( long "end"      <> short 'e' <> help endHelpMsg                        ) )
      <*> switch              ( long "chat"     <> short 'c' <> help chatHelpMsg                       )
    targetHelpMsg  = "When downloading live type stream, TARGET must be <channel name>."
                     <> "ex) kurl playhearthstone --live"
                     <> "When downloading archive type stream, TARGET must be <vod url>."
                     <> "ex) kurl https://www.twitch.tv/videos/123456789"
    liveHelpMsg    = "set type stream to live. cannot be used with --arch option."
    archHelpMsg    = "set type stream to archive. cannot be used with --live option."
    bareHelpMsg    = "Prints only m3u8 url. Useful for using url for otehr programs."
    fullHelpMsg    = "Prints full encoding command."
    qualityHelpMsg = "set stream quality. default is chunked."
                     <> " chunked is source quality. ex) chunked, 720p60, 480p30"
                     <> "ex) kurl https://www.twitch.tv/videos/123456789 --quality 720p60"
    tsHelpMsg      = "download ts files of the vod. Supported on only archive type stream."
    startHelpMsg   = "recording start offset. Format is 0h0m0s"
                     <> "ex) kurl https://www.twitch.tv/videos/123456789 --start 30m"
    endHelpMsg     = "recording end offset. Format is 0h0m0s"
                     <> "ex) kurl https://www.twitch.tv/videos/123456789 --end 1h2m3s"
    chatHelpMsg    = "download vod chat log. Supported on only archive type stream."


downloadChat :: KurlConf -> String -> Text -> NominalDiffTime -> NominalDiffTime -> IO ()
downloadChat kurlConf target user startNominalDiff endNominalDiff = do
  printf "start downloading all comments of vod: %s ...\n"  target
  logs <- getChatLogs kurlConf target startNominalDiff endNominalDiff
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
  runStream . serially $ fromFoldableM $ fmap (processTS url) [ s .. e ]


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
