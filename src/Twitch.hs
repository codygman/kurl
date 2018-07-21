{-# LANGUAGE OverloadedStrings   #-}


module Twitch
  ( makeVodBaseUrl
  ) where

import           Data.Text
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Char8  as CB
import qualified Data.ByteString.Lazy   as LB
import           Network.Wreq
import           Text.Printf
import           Control.Lens
import           Data.Function             ((&))


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





-- data VodResponse = VodResponse
--   { id            :: !Text
--   , user_id       :: !Text
--   , title         :: !Text
--   , description   :: !Text
--   , created_at    :: !Text
--   , published_at  :: !Text
--   , url           :: !Text
--   , thumbnail_url :: !Text
--   , viewable      :: !Text
--   , view_count    :: !Int
--   , language      :: !Text
--   , vodtype       :: !Text
--   , duration      :: !Text
--   } deriving (Show, Generic)

-- makeLenses ''VodResponse

-- deriveJSON defaultOptions { fieldLabelModifier = vodRespMapper } ''VodResponse

