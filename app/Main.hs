{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE DeriveGeneric       #-}


module Main where


import           Control.Monad
import           Control.Lens
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as CB
import qualified Data.ByteString.Lazy   as LB
import           Data.ByteString.Char8     (pack)
import           Data.Function             ((&))
import           Network.Wreq
import           Streamly
import qualified Streamly.Prelude       as S
import           System.Environment
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.Aeson.Lens
import           Data.Text
import           GHC.Generics
import           Text.Printf
import qualified Data.Text              as T
import qualified Data.Text.IO           as TI
import           Data.Text.Encoding

import           Rename                 (vodRespMapper)
import           Parse                  (getIdx, parseDuration)


-- https://vod109-ttvnw.akamaized.net/c3e83fffa00147116a7e_towelliee_29542954720_915104833/chunked/index-dvr.m3u8
-- https://vod.edgecast.hls.ttvnw.net/c3e83fffa00147116a7e_towelliee_29542954720_915104833/chunked/3.ts

-- {
--   "data": [{
--     "id": "234482848",
--     "user_id": "67955580",
--     "title": "-",
--     "description": "",
--     "created_at": "2018-03-02T20:53:41Z",
--     "published_at": "2018-03-02T20:53:41Z",
--     "url": "https://www.twitch.tv/videos/234482848",
--     "thumbnail_url": "https://static-cdn.jtvnw.net/s3_vods/bebc8cba2926d1967418_chewiemelodies_27786761696_805342775/thumb/thumb0-%{width}x%{height}.jpg",
--     "viewable": "public",
--     "view_count": 142,
--     "language": "en",
--     "type": "archive",
--     "duration": "3h8m33s"
--   }],
--   "pagination":{"cursor":"eyJiIjpudWxsLCJhIjoiMTUwMzQ0MTc3NjQyNDQyMjAwMCJ9"}
-- }


data VodResponse = VodResponse
  { id            :: !Text
  , user_id       :: !Text
  , title         :: !Text
  , description   :: !Text
  , created_at    :: !Text
  , published_at  :: !Text
  , url           :: !Text
  , thumbnail_url :: !Text
  , viewable      :: !Text
  , view_count    :: !Int
  , language      :: !Text
  , vodtype       :: !Text
  , duration      :: !Text
  } deriving (Show, Generic)

-- makeLenses ''VodResponse

deriveJSON defaultOptions { fieldLabelModifier = vodRespMapper } ''VodResponse


main :: IO ()
main = do
  [vodId, start, end, output] <- getArgs
  printf "start downloading vod: %s ...\n"  vodId

  vodBaseUrl <- makeVodBaseUrl vodId

  printf "vod base url => %s\n" vodBaseUrl

  let m3u8file = "index-dvr.m3u8"
  processM3U8 vodBaseUrl m3u8file

  m3u8Content <- readFile (T.unpack m3u8file)
  let sIdx = getIdx (parseDuration start) m3u8Content
      eIdx = getIdx (parseDuration end) m3u8Content
  printf "download ts range: %d ~ %d\n" sIdx eIdx

  runStream . serially $ do
    let files = [ sIdx .. eIdx ]
    S.fromFoldableM $ fmap (processTS vodBaseUrl) files

  printf "ts files download completed.\n"
  printf "ffmpeg -i %s -c:v copy -c:a copy -t %s %s\n" m3u8file end output


fetchTs :: Text ->  IO B.ByteString
fetchTs url = do
  printf "download => %s\n" url
  response <- get . T.unpack $ url
  return . LB.toStrict $ response ^. responseBody


writeTs :: Text -> B.ByteString -> IO ()
writeTs filename  bs = do
  printf "writing => %s\n"  filename
  B.writeFile (T.unpack filename) bs


processFile :: Text -> Text -> IO ()
processFile vodBaseUrl tsName  = do
  let fullUrl = makeDnUrl vodBaseUrl tsName
  fetchTs fullUrl >>= writeTs tsName


processM3U8 :: Text -> Text -> IO ()
processM3U8 vodBaseUrl m3u8Filename = do
  printf "downloading => %s%s\n" vodBaseUrl m3u8Filename
  processFile vodBaseUrl m3u8Filename


processTS :: Text -> Int -> IO ()
processTS vodBaseUrl tsIdx = do
  let tsName  = T.pack $ show tsIdx <> ".ts"
  processFile vodBaseUrl tsName


concatBytes :: B.ByteString -> B.ByteString -> IO B.ByteString
concatBytes a b = B.append <*> return a <$> return b


makeDnUrl :: Text -> Text -> Text
makeDnUrl vodBaseUrl tsName = "https://vod.edgecast.hls.ttvnw.net/" <> vodBaseUrl <> "/chunked/" <> tsName


makeVodBaseUrl :: String -> IO Text
makeVodBaseUrl vodId = do
  thumbnail <- getThumbnailUrl "https://api.twitch.tv/helix/videos" "g9r0psjr0nn0a4ypjh62b6p568jhom" vodId
  return $ extractBaseUrl thumbnail


getThumbnailUrl :: String -> String -> String -> IO Text
getThumbnailUrl apiUrl clientId vodId = do
  let url  :: String
      url   = printf "%s?id=%s" apiUrl vodId
      opts  = defaults & header "Client-ID" .~ [ CB.pack clientId ]
  response <- getWith opts url :: IO (Response LB.ByteString)
  return $ response ^. responseBody
                      . key "data"
                      . nth 0
                      . key "thumbnail_url"
                      . _String

extractBaseUrl :: Text -> Text
extractBaseUrl url = split (== '/' ) url !! 4

-- extractBaseUrl :: Text -> Text
-- extractBaseUrl url = url
