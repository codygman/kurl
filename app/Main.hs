{-# LANGUAGE OverloadedStrings   #-}


module Main where

import           System.Environment     (getArgs)
import           Text.Printf            (printf)

import           Streamly
import qualified Streamly.Prelude       as S
import qualified Data.Text              as T

import           Parse                  (getIdx, parseDuration, formatDuration)
import           Twitch                 (getVideoInfo, getVideoAllComments, mkTwitchCfg, VideoInfo(..))
import           TsIO                   (processM3U8, processTS, writeComments)
import           Control.Monad.Reader   (runReaderT)
import           Data.Maybe
import           Data.Time
import           Options.Applicative


data CmdOpts = CmdOpts
  { vodId :: String
  , start :: String
  , end   :: String
  , chat  :: Bool
  }


cmd :: Parser CmdOpts
cmd = CmdOpts
  <$> strOption ( long "vod"   <> short 'v' <> metavar "TARGET" <> help "download TARGET videoid" )
  <*> strOption ( long "start" <> short 's' <> help "recording start offset" )
  <*> strOption ( long "end"   <> short 'e' <> help "recording end offset" )
  <*> switch    ( long "chat"  <> short 'c' <> help "download vod chat log" )


main :: IO ()
main = main' =<< execParser opts
  where
    opts = info ( cmd <**> helper )
                (fullDesc <> progDesc "Download twitch vod TARGET" <> header "kurl - a twitch vod downloader")


main' :: CmdOpts -> IO ()
main' (CmdOpts vodId start end chat) = do
  let cfg = mkTwitchCfg "https://api.twitch.tv/helix/" "g9r0psjr0nn0a4ypjh62b6p568jhom"

  let (startUTC, endUTC) =
        case ((,) <$> parseDuration start <*> parseDuration end) of
          Just (startUTC, endUTC) -> (startUTC, endUTC)
          _ -> error "start end time option error time format should be: <0h0m0s|0h0m|0m0s|0h|0m|0s>"

  videoInfo  <- runReaderT (getVideoInfo vodId) cfg
  let vodBaseUrl  = videoinfo_baseUrl videoInfo
      vodUserName = videoinfo_userDisplayName videoInfo

  _ <- if not chat then return () else do
    printf "start downloading all comments of vod: %s ...\n"  vodId
    videoComments <- runReaderT (getVideoAllComments vodId) cfg
    writeComments vodId vodUserName videoComments

  printf "start downloading ts files of vod: %s ...\n"  vodId

  printf "vod base url => %s\n" vodBaseUrl

  let startTime = formatDuration startUTC
      endTime   = formatDuration endUTC
      m3u8file  = "index-dvr.m3u8"
      fileExt   = "mp4" :: T.Text
      mp4file   = T.pack $ printf "%s_%s_%s_%s.%s" vodUserName vodId startTime endTime fileExt :: T.Text

  processM3U8 vodBaseUrl m3u8file

  m3u8Content <- readFile (T.unpack m3u8file)
  let sIdx = getIdx startUTC m3u8Content
      eIdx = getIdx endUTC m3u8Content
  printf "download ts range: %d ~ %d\n" sIdx eIdx

  runStream . serially $ do
    let files = [ sIdx .. eIdx ]
    S.fromFoldableM $ fmap (processTS vodBaseUrl) files

  printf "ts files download completed.\n"
  printf "ffmpeg -i %s -c:v copy -c:a copy -t %s %s\n" m3u8file endTime mp4file


