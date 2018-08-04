{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleContexts   #-}


module Twitch
  ( twitchAPI
  , getVideoInfo
  , mkTwitchCfg
  , getVideoComment
  , getChatLogs
  , comment_message
  , comment_commenter
  , VideoInfo(..)
  , Comment(..)
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
import           GHC.Generics           (Generic)
import           Control.Monad.Reader
-- import           Control.Monad.Reader.Class
import           Data.Maybe (fromMaybe)
import           Data.Time
import           Parse (parseDuration)


newtype TwitchData a = TwitchData
  { _twitch_data :: [a]
  } deriving (Show, Generic)


data Video = Video
  { _video_id            :: !Text
  , _video_user_id       :: !Text
  , _video_title         :: !Text
  , _video_description   :: !Text
  , _video_created_at    :: !Text
  , _video_published_at  :: !Text
  , _video_url           :: !Text
  , _video_thumbnail_url :: !Text
  , _video_viewable      :: !Text
  , _video_view_count    :: !Int
  , _video_language      :: !Text
  , _video_type          :: !Text
  , _video_duration      :: !Text
  } deriving (Show, Generic)


data User = User
  { _user_id                :: !Text
  , _user_login             :: !Text
  , _user_display_name      :: !Text
  , _user_type              :: !Text
  , _user_broadcaster_type  :: !Text
  , _user_description       :: !Text
  , _user_profile_image_url :: !Text
  , _user_offline_image_url :: !Text
  , _user_view_count        :: !Int
  , _user_email             :: Maybe Text
  } deriving (Show, Generic)


newtype CommentData = CommentData
  { _comment_data :: [Comment]
  } deriving (Show, Generic)


data Comment = Comment
  { _comment__id                    :: !Text
  , _comment_created_at             :: !Text
  , _comment_updated_at             :: !Text
  , _comment_channel_id             :: !Text
  , _comment_content_type           :: !Text
  , _comment_content_id             :: !Text
  , _comment_content_offset_seconds :: !Float
  , _comment_commenter              :: !Commenter
  , _comment_source                 :: !Text
  , _comment_state                  :: !Text
  , _comment_message                :: !Message
  , _comment_more_replies           :: !Bool
  } deriving (Show, Generic)


data Commenter = Commenter
  { _commenter_display_name :: !Text
  , _commenter__id          :: !Text
  , _commenter_name         :: !Text
  , _commenter_type         :: !Text
  , _commenter_bio          :: Maybe Text
  , _commenter_created_at   :: !Text
  , _commenter_updated_at   :: !Text
  , _commenter_logo         :: Maybe Text
  } deriving (Show, Generic)


data Message = Message
  { _message_body        :: !Text
  , _message_fragments   :: [Fragment]
  , _message_is_action   :: !Bool
  , _message_user_badges :: Maybe [UserBadge]
  , _message_user_color  :: Maybe Text
  } deriving (Show, Generic)


newtype Fragment = Fragment
  { _fragment_text :: Text
  } deriving (Show, Generic)


data UserBadge = UserBadge
  { _userbadge__id     :: !Text
  , _userbadge_version :: !Text
  } deriving (Show, Generic)


deriveJSON defaultOptions { fieldLabelModifier = const "data"                } ''TwitchData
deriveJSON defaultOptions { fieldLabelModifier = const "comments"            } ''CommentData
deriveJSON defaultOptions { fieldLabelModifier = drop (length "_video_")     } ''Video
deriveJSON defaultOptions { fieldLabelModifier = drop (length "_user_")      } ''User
deriveJSON defaultOptions { fieldLabelModifier = drop (length "_comment_")   } ''Comment
deriveJSON defaultOptions { fieldLabelModifier = drop (length "_commenter_") } ''Commenter
deriveJSON defaultOptions { fieldLabelModifier = drop (length "_message_")   } ''Message
deriveJSON defaultOptions { fieldLabelModifier = drop (length "_fragment_")  } ''Fragment
deriveJSON defaultOptions { fieldLabelModifier = drop (length "_userbadge_") } ''UserBadge


makeLenses ''TwitchData
makeLenses ''Video
makeLenses ''User
makeLenses ''CommentData
makeLenses ''Comment
makeLenses ''Commenter
makeLenses ''Message
makeLenses ''Fragment
makeLenses ''UserBadge


data VideoInfo = VideoInfo
  { videoinfo_baseUrl :: Text
  , videoinfo_userDisplayName :: Text
  } deriving (Show)


data TwitchCfg = TwitchCfg
  { twitchcfg_clientid :: !Text
  , twitchcfg_endpoint :: !Text
  }


mkTwitchCfg :: Text -> Text -> TwitchCfg
mkTwitchCfg endpoint clientid =
  TwitchCfg
    { twitchcfg_clientid = clientid
    , twitchcfg_endpoint = endpoint
    }


getVideoInfo :: (MonadIO m, MonadReader TwitchCfg m) => String -> m VideoInfo
getVideoInfo _videoId = do
  videoResp <- twitchAPI "videos" (decodeUtf8 . CB.pack $ _videoId)
  let twitchData = videoResp ^. responseBody . twitch_data
      userId     = twitchData ^?! traverse . video_user_id
      baseUrl    = twitchData ^?! traverse . video_thumbnail_url . to extractBaseUrl
  usersResp <- twitchAPI "users" userId
  let username =  usersResp ^. responseBody . twitch_data ^?! traverse . user_display_name
  return $ VideoInfo { videoinfo_baseUrl         = baseUrl
                     , videoinfo_userDisplayName = username
                     }


twitchAPI :: (MonadIO m, MonadReader TwitchCfg m, FromJSON a) => Text -> Text -> m (Response (TwitchData a))
twitchAPI apiKind idParam = do
  cfg <- ask
  let apiUrl   = twitchcfg_endpoint cfg <> apiKind
      clientId = twitchcfg_clientid cfg
      url  :: String
      url   = printf "%s?id=%s" apiUrl idParam
      opts  = defaults & header "Client-ID" .~ [ encodeUtf8 clientId ]
  liftIO $ asJSON =<< getWith opts url


getVideoComment :: (MonadIO m, MonadReader TwitchCfg m) => String -> Float -> m [Comment]
getVideoComment _videoId offset = do
  cfg <- ask
  let clientId = twitchcfg_clientid cfg
      url  :: String
      url   = printf "https://api.twitch.tv/v5/videos/%s/comments?content_offset_seconds=%f" _videoId offset
      opts  = defaults & header "Client-ID" .~ [ encodeUtf8 clientId ]
                       & header "Content-Type" .~ [ "application/vnd.twitchtv.v5+json" ]
  resp <- liftIO $ asJSON =<< getWith opts url
  return $ resp ^. responseBody . comment_data


getChatLogs :: (MonadIO m, MonadReader TwitchCfg m) => String -> UTCTime -> UTCTime -> m [(Text, Text, Text)]
getChatLogs vodId startUTC endUTC = do
  comments <- getChatLogs' ssec ssec
  let time = Getter $ comment_created_at
      name = Getter $ comment_commenter . commenter_display_name
      mesg = Getter $ comment_message . message_body
  return $ comments ^.. traverse . runGetter ((,,) <$> time <*> name <*> mesg)
  where
    ssec = fromRational . toRational . utctDayTime $ startUTC
    esec = fromRational . toRational . utctDayTime $ endUTC
    getChatLogs' psec csec = do
      comments <- getVideoComment vodId csec
      let nsec = fromMaybe csec $ lastOf (traverse . comment_content_offset_seconds) comments
      liftIO $ printf "downloading chat comment from %f seconds\n" csec
      restOfComments <- if csec == nsec || esec < nsec  then return [] else getChatLogs' csec nsec
      return $ comments ++ restOfComments


extractBaseUrl :: Text -> Text
extractBaseUrl url = split (== '/' ) url !! 4