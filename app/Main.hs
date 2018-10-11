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


data ArgType = MainCmd | MainArg | Quality | Start | End


type Target = String


main :: IO ()
main = do
  args <- getArgs
  kurlConf <- Dh.input Dh.auto "~/.config/kurl/kurl.conf" :: IO KurlConf
  let mainCmd = parseArgs MainCmd args
      mainArg = parseArgs MainArg args
      quality = parseArgs Quality args
      start   = parseArgs Start   args
      end     = parseArgs End     args
  case (mainCmd, mainArg) of
    (Just "list" , _            ) -> case (pack <$> mainArg) <|> kurlConfUserLoginName kurlConf of
                                       Just mainArg' -> listAction kurlConf mainArg'
                                       Nothing       -> printf "No or incorrect user argument for list commnad.\n"
    (Just "chat" , Just mainArg') -> chatAction kurlConf mainArg' start end
    (Just "m3u"  , Just mainArg') -> m3uAction  kurlConf mainArg' quality
    (Just "enc"  , Just mainArg') -> encAction  kurlConf mainArg' quality start end
    (Just "ver"  , _            ) -> printf "version 1.3\n"
    (Just unknown, Just _       ) -> printf "Uknown command %s\n" unknown
    (Nothing     , Just _       ) -> printf "No main command\n"
    (Just _      , Nothing      ) -> printf "Incorrect usage or no argument: `%s`\n" (Data.List.intercalate " " args)
    _                             -> printf "Available commands: list m3u enc chat ver\n"


parseArgs :: ArgType -> [String] -> Maybe String
parseArgs argType args =
  case argType of
    MainCmd -> mainCmdArgSanityCheck
    MainArg -> mainArgSanityCheck
    Quality -> optionalArgsPosSanityCheck (isPrefixOf "q=")
    Start   -> optionalArgsPosSanityCheck (isPrefixOf "s=")
    End     -> optionalArgsPosSanityCheck (isPrefixOf "e=")
  where
    mainCmdArgSanityCheck =
      case args ^? ix 0 of 
        Just "list"  -> Just "list"
        Just "chat"  -> Just "chat"
        Just "m3u"   -> Just "m3u"
        Just "enc"   -> Just "enc"
        Just "ver"   -> Just "ver"
        _            -> Nothing
    mainArgSanityCheck =
      let mainArg = args ^? ix 1
      in case mainArg of
           Just arg -> case mainCmdArgSanityCheck of
                         Just "list" -> if isUser arg then mainArg else Nothing
                         Just "enc"  -> if isVod arg || isUser arg then mainArg else Nothing
                         Just "m3u"  -> if isVod arg || isUser arg then mainArg else Nothing
                         Just "chat" -> if isVod  arg then extractVodId <$> mainArg else Nothing
                         _           -> Nothing
           Nothing  -> Nothing
      where
        isVod  inp = "https://www.twitch.tv/videos" `isPrefixOf` inp && all isNumber (extractVodId inp)
        isUser inp = all isAlphaNum inp

    optionalArgsPosSanityCheck argPred =
      if findIndexOf folded argPred args >= Just 2
        then extractValue <$> findOf folded argPred args
        else Nothing
    extractValue =  reverse . takeWhile (/= '=') . reverse
    extractVodId =  reverse . takeWhile (/= '/') . reverse


listAction :: KurlConf -> Text -> IO ()
listAction kurlConf currentUserLoginName = do
  liveStreams <- getLiveStreamList kurlConf currentUserLoginName
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
