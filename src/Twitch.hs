{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TemplateHaskell   #-}


module Twitch
  ( twitchAPI
  , getVideoInfo
  , VideoInfo(..)
  ) where


import           Prelude                hiding (length)
import           Data.Text              hiding (drop)
import           Data.Text.Encoding        (encodeUtf8, decodeUtf8)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Lens
import qualified Data.ByteString.Char8  as CB
import qualified Data.ByteString.Lazy   as LB
import           Network.Wreq
import           Text.Printf
import           Control.Lens
import           Data.Function             ((&))
import           Data.Monoid               ((<>))
import           GHC.Generics


newtype TwitchResp a = TwitchResp
  { twitchresp_data :: [a]
  } deriving (Show ,Generic)


data VodResp = VodResp
  { vodresp_id            :: !Text
  , vodresp_user_id       :: !Text
  , vodresp_title         :: !Text
  , vodresp_description   :: !Text
  , vodresp_created_at    :: !Text
  , vodresp_published_at  :: !Text
  , vodresp_url           :: !Text
  , vodresp_thumbnail_url :: !Text
  , vodresp_viewable      :: !Text
  , vodresp_view_count    :: !Int
  , vodresp_language      :: !Text
  , vodresp_type          :: !Text
  , vodresp_duration      :: !Text
  } deriving (Show, Generic)


data UserResp = UserResp
  { userresp_id                :: !Text
  , userresp_login             :: !Text
  , userresp_display_name      :: !Text
  , userresp_type              :: !Text
  , userresp_broadcaster_type  :: !Text
  , userresp_description       :: !Text
  , userresp_profile_image_url :: !Text
  , userresp_offline_image_url :: !Text
  , userresp_view_count        :: !Int
  , userresp_email             :: !Text
  } deriving (Show, Generic)


deriveJSON defaultOptions { fieldLabelModifier = const "data"              } ''TwitchResp
deriveJSON defaultOptions { fieldLabelModifier = drop (length "vodresp_")  } ''VodResp
deriveJSON defaultOptions { fieldLabelModifier = drop (length "userresp_") } ''UserResp


data VideoInfo = VideoInfo
  { videoinfo_baseUrl :: Text
  , videoinfo_userDisplayName :: Text
  } deriving (Show)


getVideoInfo :: String -> IO VideoInfo
getVideoInfo vodId = do
  let apiKey = "g9r0psjr0nn0a4ypjh62b6p568jhom"
      endPoint = "https://api.twitch.tv/helix/"

  resp <- twitchAPI (endPoint  <> "videos") apiKey (decodeUtf8 . CB.pack $ vodId)
  let vodResp = twitchresp_data (resp ^. responseBody) !! 0
      userId  = vodresp_user_id $ vodResp
      baseUrl = extractBaseUrl . vodresp_thumbnail_url $ vodResp

  resp <- twitchAPI (endPoint  <> "users") apiKey userId
  let userResp = twitchresp_data (resp ^. responseBody) !! 0
      username = userresp_display_name userResp

  return $ VideoInfo { videoinfo_baseUrl         = baseUrl
                     , videoinfo_userDisplayName = username
                     }


twitchAPI :: FromJSON a => Text -> Text -> Text -> IO (Response (TwitchResp a))
twitchAPI apiUrl clientId idParam = do
  let url  :: String
      url   = printf "%s?id=%s" apiUrl idParam
      opts  = defaults & header "Client-ID" .~ [ encodeUtf8 clientId ]
  asJSON =<< getWith opts url


extractBaseUrl :: Text -> Text
extractBaseUrl url = split (== '/' ) url !! 4
