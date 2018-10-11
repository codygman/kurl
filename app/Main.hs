{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase   #-}


module Main where


import           Control.Applicative         ((<|>))
import           Control.Lens
import           Data.Char                   (isNumber, isAlphaNum)
import           Data.Maybe                  (fromJust, fromMaybe)
import           Data.List                   (isPrefixOf, intercalate)
import           Data.Time                   (NominalDiffTime, getCurrentTime)
import           Data.Text                   (Text, pack, unpack, intercalate)
import qualified Dhall                    as Dh
import           System.Info                 (os)
import           Text.Printf                 (printf)
import           Streamly                    (runStream, serially)
import           Streamly.Prelude            (fromFoldableM)
import           System.Environment

import           M3u8                        (getStartIdx, getEndIdx,)
import           TimeFormat                  (format4file, formatUtc, format4ffmpeg, makeOffset)
import           TsIO                        (processM3U8, processTS, writeComments)
import           Twitch                      (getLive, getArchive, getChatLogs, getLiveStreamList, VideoInfo(..))
import           Conf                        (KurlConf(..))


data ArgType = Quality | Start | End

data Command = CmdList
  | CmdM3u
  | CmdEnc
  | CmdChat
  | CmdVer
  | CmdUnknown String
  | CmdNothing
  deriving (Show)


data ArgMain = ArgList String
  | ArgM3u String
  | ArgEnc String
  | ArgChat String
  | ArgVer
  | ArgMalformed String
  | ArgUnknownCmd String
  | ArgNothing
  | ArgMissing


type Target = String


main :: IO ()
main = do
  args <- getArgs
  kurlConf <- Dh.input Dh.auto "~/.config/kurl/kurl.conf" :: IO KurlConf
  let mainCmd = parseMainCmd args 
      mainArg = parseMainArg kurlConf mainCmd args
      quality = parseOptionalArgs Quality args
      start   = parseOptionalArgs Start args
      end     = parseOptionalArgs End args
  case mainArg of
    ArgList arg           -> listAction kurlConf arg
    ArgChat arg           -> chatAction kurlConf arg start end
    ArgM3u arg            -> m3uAction kurlConf arg quality
    ArgEnc arg            -> encAction kurlConf arg quality start end
    ArgMalformed arg      -> printf "Malformed argument %s\n for command %s" arg (show mainCmd)
    ArgUnknownCmd unknown -> printf "Unknown command %s\n" unknown 
    ArgVer                -> printf "version 1.3\n"
    ArgMissing            -> printf "Argument is missing for command %s\n" (show mainCmd)
    ArgNothing            -> printf "Available commands: list m3u enc chat ver\n"


parseOptionalArgs :: ArgType -> [String] -> Maybe String
parseOptionalArgs argType args =
  case argType of
    Quality -> optionalArgsPosSanityCheck (isPrefixOf "q=")
    Start   -> optionalArgsPosSanityCheck (isPrefixOf "s=")
    End     -> optionalArgsPosSanityCheck (isPrefixOf "e=")
  where
    optionalArgsPosSanityCheck argPred =
      if findIndexOf folded argPred args >= Just 2
        then extractValue <$> findOf folded argPred args
        else Nothing
    extractValue =  reverse . takeWhile (/= '=') . reverse


parseMainCmd :: [String] -> Command
parseMainCmd args = case args ^? ix 0 of
  Just "list"  -> CmdList
  Just "enc"   -> CmdEnc
  Just "m3u"   -> CmdM3u
  Just "chat"  -> CmdChat
  Just "ver"   -> CmdVer
  Just unknown -> CmdUnknown unknown
  Nothing      -> CmdNothing


parseMainArg :: KurlConf -> Command -> [String] -> ArgMain
parseMainArg kurlConf command args =
  case args ^? ix 1 of
    Just arg -> case command of
      CmdList        -> if isUser arg then ArgList arg else ArgMalformed arg
      CmdEnc         -> if isVod arg || isUser arg then ArgEnc arg else ArgMalformed arg
      CmdM3u         -> if isVod arg || isUser arg then ArgM3u arg else ArgMalformed arg
      CmdChat        -> if isVod arg then ArgChat $ extractVodId arg else ArgMalformed arg
      CmdUnknown cmd -> ArgUnknownCmd cmd
    Nothing -> case command of
      CmdList           -> case kurlConfUserLoginName kurlConf of
                             Just loginName -> ArgList $ unpack loginName
                             Nothing        -> ArgMissing
      CmdNothing         -> ArgNothing
      CmdVer             -> ArgVer
      CmdUnknown unknown -> ArgUnknownCmd unknown
      _                  -> ArgMissing
    where
      isVod  inp   = "https://www.twitch.tv/videos" `isPrefixOf` inp && all isNumber (extractVodId inp)
      isUser inp   = all isAlphaNum inp
      extractVodId =  reverse . takeWhile (/= '/') . reverse


listAction :: KurlConf -> String -> IO ()
listAction kurlConf currentUserLoginName = do
  liveStreams <- getLiveStreamList kurlConf (pack currentUserLoginName)
  mapM_ (printf "%s\n" . unpack) liveStreams


m3uAction :: KurlConf -> Target -> Maybe String -> IO ()
m3uAction kurlConf target quality = do
    let quality'             = fromMaybe "chunked" quality
        (isArchive, target') = parseVodUrl target
    (VideoInfo fullUrl _ _) <- if isArchive
      then getArchive kurlConf quality' target'
      else getLive    kurlConf quality' target'
    printf "%s" fullUrl


encAction :: KurlConf -> Target -> Maybe String -> Maybe String -> Maybe String -> IO ()
encAction kurlConf target quality start end = do
    let quality' = (fromMaybe "chunked" quality)
        (isArchive, target') = parseVodUrl target
    if isArchive then do
        (VideoInfo fullUrl user duration) <- getArchive kurlConf quality' target'
        let defaultStart        = "00:00:00"
            startNominalDiff    = makeOffset $ fromMaybe defaultStart start
            endNominalDiff      = makeOffset $ fromMaybe (unpack . fromJust $ duration) end
            durationNominalDiff = endNominalDiff - startNominalDiff
        printEncodingCmdArchive target user startNominalDiff durationNominalDiff endNominalDiff fullUrl
    else do
      (VideoInfo fullUrl _ _) <- getLive kurlConf quality' target'
      printEncodingCmdLive target fullUrl


chatAction :: KurlConf -> Target -> Maybe String -> Maybe String -> IO ()
chatAction kurlConf target start end = do
    (VideoInfo _ user duration) <- getArchive kurlConf "chunked" target
    let defaultStart        = "00:00:00"
        startNominalDiff    = makeOffset $ fromMaybe defaultStart start
        endNominalDiff      = makeOffset $ fromMaybe (unpack . fromJust $ duration) end
    printf "Start downloading chat...\n"
    downloadChat kurlConf target user startNominalDiff endNominalDiff


-- TODO: There's user name only consisted with numbers.
--       How we can differentiate with vod and channel name?
--  FIX: Just don't accept target input which only consisted with numbers as vod id.
parseVodUrl :: String -> (Bool, String)
parseVodUrl strInp = let target = reverse . takeWhile (/= '/') . reverse $ strInp
                     in if all isNumber target && "https://www.twitch.tv/videos" `isPrefixOf` strInp
                          then  (True, target)
                          else  (False, target)


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


printEncodingCmdArchive :: Target -> Text -> NominalDiffTime -> NominalDiffTime -> NominalDiffTime -> Text -> IO ()
printEncodingCmdArchive target user s d e m3u8 = do
  let offset    = format4ffmpeg s
      duration  = format4ffmpeg d
      ext       = "mp4" :: String
      mp4       = pack $ printf "%s_%s_%s_%s.%s" user target (format4file s) (format4file e) ext
      formatStr = unpack $ Data.Text.intercalate delim ["ffmpeg", "-ss %s", "-i %s", "-to %s", "-c:v copy","-c:a copy", "%s\n"]
  printf formatStr offset m3u8 duration mp4


printEncodingCmdLive :: Target ->  Text -> IO ()
printEncodingCmdLive channelId m3u8 = do
  cutc <- getCurrentTime
  let ext       = "mp4" :: String
      mp4       = pack $ printf "%s_live_%s.%s" channelId (formatUtc cutc) ext
      formatStr = unpack $ Data.Text.intercalate delim ["ffmpeg", "-i %s", "-c:v copy","-c:a copy", "%s\n"]
  printf formatStr m3u8 mp4


delim :: Text
delim  = if os == "mingw32" || os == "mingw64" then " ^\n" else " \\\n"
