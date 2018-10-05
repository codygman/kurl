{-# LANGUAGE OverloadedStrings   #-}


module Main where


import           Control.Monad                            ( when )
import           Data.Char                                ( isNumber )
import           Data.Maybe                               ( fromJust
                                                          , fromMaybe
                                                          , isJust
                                                          )
import           Data.List                                ( isPrefixOf )
import           Data.Time                                ( NominalDiffTime
                                                          , getCurrentTime
                                                          )
import           Data.Text                                ( Text
                                                          , pack
                                                          , unpack
                                                          , intercalate
                                                          )
import           Options.Applicative
import           System.Info                              ( os )
import           Text.Printf                              ( printf )
import           Streamly                                 ( runStream
                                                          , serially
                                                          )
import           Streamly.Prelude                         ( fromFoldableM )

import           M3u8                                     ( getStartIdx
                                                          , getEndIdx
                                                          )
import           TimeFormat                               ( format4file
                                                          , formatUtc
                                                          , format4ffmpeg
                                                          , makeOffset
                                                          )
import           TsIO                                     ( processM3U8
                                                          , processTS
                                                          , writeComments
                                                          )
import           Twitch                                   ( getLive
                                                          , getArchive
                                                          , getChatLogs
                                                          , getLiveStreamList
                                                          , VideoInfo(..)
                                                          )


data CmdOpts = CmdOpts
  { mainArg :: Maybe String
  , quality :: String
  , ts      :: Bool
  , start   :: Maybe String
  , end     :: Maybe String
  , chat    :: Bool
  , ffmpeg  :: Bool
  , list    :: Bool
  , version :: Bool
  }

data MainCmd = Target String | List String


main :: IO ()
main = do
  cmdOpts <- parseCmdOpts
  if version cmdOpts
    then printf "kurl 1.2\n"
    else if isJust . mainArg $ cmdOpts
      then if list cmdOpts then queryAction cmdOpts else downloadAction cmdOpts
      else printf "Missing TARGET\n"


queryAction :: CmdOpts -> IO ()
queryAction cmdOpts = do
  liveStreams <- getLiveStreamList (pack . fromJust . mainArg $ cmdOpts)
  mapM_ (printf "%s\n" . unpack) liveStreams


downloadAction :: CmdOpts -> IO ()
downloadAction cmdOpts = do
  let (notLive, target) = parseVodUrl (fromJust . mainArg $ cmdOpts)

  if notLive
    then do
      (VideoInfo fullUrl user duration) <- getArchive (quality cmdOpts) target
      let defaultStart = "00:00:00"
          startNominalDiff =
            makeOffset $ fromMaybe defaultStart (start cmdOpts)
          endNominalDiff =
            makeOffset $ fromMaybe (unpack . fromJust $ duration) (end cmdOpts)
          durationNominalDiff = endNominalDiff - startNominalDiff
      if ffmpeg cmdOpts
        then do
          printEncodingCmdArchive target
                                  user
                                  startNominalDiff
                                  durationNominalDiff
                                  endNominalDiff
                                  fullUrl

          when (chat cmdOpts) $ do
            printf "Start downloading chat...\n"
            downloadChat target user startNominalDiff endNominalDiff

          when (ts cmdOpts) $ do
            -- TODO: this is not proper file name for index-dvr.m3u8
            --       this must be extracted from full url.
            let localIndexDvrM3u8 = "index-dvr.m3u8"
            downloadVod target
                        fullUrl
                        startNominalDiff
                        endNominalDiff
                        localIndexDvrM3u8
        else printf "%s" fullUrl
    else do
      VideoInfo fullUrl user _ <- getLive (quality cmdOpts) target
      if ffmpeg cmdOpts
        then printEncodingCmdLive user fullUrl
        else printf "%s" fullUrl


-- TODO: There's user name only consisted with numbers.
--       How we can differentiate with vod and channel name?
--  FIX: Just don't accept target input which only consisted with numbers as vod id.
parseVodUrl :: String -> (Bool, String)
parseVodUrl strInp
  = let target = reverse . takeWhile (/= '/') . reverse $ strInp
    in
      if all isNumber target
         &&           "https://www.twitch.tv/videos"
         `isPrefixOf` strInp
      then
        (True, target)
      else
        (False, target)


parseCmdOpts :: IO CmdOpts
parseCmdOpts = execParser $ info
  (cmd <**> helper)
  (  fullDesc
  <> progDesc "Download twitch TARGET. TARGET is <vod url> or <channel name>."
  <> header "kurl - a twitch vod downloader"
  )
 where
  cmd =
    CmdOpts
      <$> optional (strArgument (metavar "TARGET" <> help targetHelpMsg))
      <*> strOption
            (long "quality" <> short 'q' <> help qualityHelpMsg <> value
              "chunked"
            )
      <*> switch (long "ts" <> short 't' <> help tsHelpMsg)
      <*> optional (strOption (long "start" <> short 's' <> help startHelpMsg))
      <*> optional (strOption (long "end" <> short 'e' <> help endHelpMsg))
      <*> switch (long "chat" <> short 'c' <> help chatHelpMsg)
      <*> switch (long "ffmpeg" <> short 'f' <> help ffmpegHelpMsg)
      <*> switch (long "list" <> short 'l' <> help listHelpMsg)
      <*> switch (long "version" <> short 'v' <> help versionHelpMsg)
  targetHelpMsg
    = " When downloading live type stream, TARGET must be \
                     \ <channel name>. ex) kurl playhearthstone.         \
                     \ When downloading archive type stream, TARGET must \
                     \ be <vod url>. ex) kurl https://www.twitch.tv/videos/123456789"
  qualityHelpMsg =
    "set stream quality. default is chunked."
      <> " chunked is source quality. ex) chunked, 720p60, 480p30"
      <> "ex) kurl https://www.twitch.tv/videos/123456789 --quality 720p60"
  tsHelpMsg =
    "download ts files of the vod. Supported on only archive type stream."
  startHelpMsg =
    "recording start offset. Format is 0h0m0s"
      <> "ex) kurl https://www.twitch.tv/videos/123456789 --start 30m"
  endHelpMsg =
    "recording end offset. Format is 0h0m0s"
      <> "ex) kurl https://www.twitch.tv/videos/123456789 --end 1h2m3s"
  chatHelpMsg = "download vod chat log. Supported on only archive type stream."
  ffmpegHelpMsg  = "Prints ffmpeg command for downloading."
  listHelpMsg    = "query current live streams which USER is following."
  versionHelpMsg = "prints current kurl version."


downloadChat :: String -> Text -> NominalDiffTime -> NominalDiffTime -> IO ()
downloadChat target user startNominalDiff endNominalDiff = do
  printf "start downloading all comments of vod: %s ...\n" target
  logs <- getChatLogs target startNominalDiff endNominalDiff
  writeComments target user logs


downloadVod
  :: String -> Text -> NominalDiffTime -> NominalDiffTime -> Text -> IO ()
downloadVod target url startNominalDiff endNominalDiff m3u8 = do
  printf "start downloading ts files of vod: %s ...\n" target
  printf "vod base url => %s\n"                        url
  -- Downloading index-dvr.m3u8
  processM3U8 url m3u8
  content <- readFile (unpack m3u8)
  -- Calculate proper range
  let s = getStartIdx startNominalDiff content
      e = getEndIdx endNominalDiff content
  printf "download ts range: %d ~ %d\n" s e
  -- Actual download of ts files
  runStream . serially $ fromFoldableM $ fmap (processTS url) [s .. e]


printEncodingCmdArchive
  :: String
  -> Text
  -> NominalDiffTime
  -> NominalDiffTime
  -> NominalDiffTime
  -> Text
  -> IO ()
printEncodingCmdArchive target user s d e m3u8 = do
  let
    offset   = format4ffmpeg s
    duration = format4ffmpeg d
    ext      = "mp4" :: String
    mp4      = pack $ printf "%s_%s_%s_%s.%s"
                             user
                             target
                             (format4file s)
                             (format4file e)
                             ext
    formatStr = unpack $ intercalate
      delim
      ["ffmpeg", "-ss %s", "-i %s", "-to %s", "-c:v copy", "-c:a copy", "%s\n"]
  printf formatStr offset m3u8 duration mp4


printEncodingCmdLive :: Text -> Text -> IO ()
printEncodingCmdLive channelId m3u8 = do
  cutc <- getCurrentTime
  let ext       = "mp4" :: String
      mp4       = pack $ printf "%s_live_%s.%s" channelId (formatUtc cutc) ext
      formatStr = unpack $ intercalate
        delim
        ["ffmpeg", "-i %s", "-c:v copy", "-c:a copy", "%s\n"]
  printf formatStr m3u8 mp4


delim :: Text
delim = if os == "mingw32" || os == "mingw64" then " ^\n" else " \\\n"
