{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleContexts   #-}


module Twitch
  ( twitchAPI
  , getVideoInfo
  , mkTwitchCfg
  , getVideoComment
  , getVideoCommentOfUser
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
import           Control.Monad.Reader
-- import           Control.Monad.Reader.Class



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


newtype CommentResp = CommentResp
  { commentresp_data :: [Comment]
  } deriving (Show ,Generic)


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
  , _commenter_logo         :: !Text
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


deriveJSON defaultOptions { fieldLabelModifier = const "data"               } ''TwitchResp
deriveJSON defaultOptions { fieldLabelModifier = const "comments"           } ''CommentResp
deriveJSON defaultOptions { fieldLabelModifier = drop (length "vodresp_")   } ''VodResp
deriveJSON defaultOptions { fieldLabelModifier = drop (length "userresp_")  } ''UserResp
deriveJSON defaultOptions { fieldLabelModifier = drop (length "_comment_")   } ''Comment
deriveJSON defaultOptions { fieldLabelModifier = drop (length "_commenter_") } ''Commenter
deriveJSON defaultOptions { fieldLabelModifier = drop (length "_message_")   } ''Message
deriveJSON defaultOptions { fieldLabelModifier = drop (length "_fragment_")  } ''Fragment
deriveJSON defaultOptions { fieldLabelModifier = drop (length "_userbadge_") } ''UserBadge


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
getVideoInfo vodId = do
  resp <- twitchAPI "videos" (decodeUtf8 . CB.pack $ vodId)
  let vodResp = twitchresp_data (resp ^. responseBody) !! 0
      userId  = vodresp_user_id $ vodResp
      baseUrl = extractBaseUrl . vodresp_thumbnail_url $ vodResp

  resp <- twitchAPI "users" userId
  let userResp = twitchresp_data (resp ^. responseBody) !! 0
      username = userresp_display_name userResp

  return $ VideoInfo { videoinfo_baseUrl         = baseUrl
                     , videoinfo_userDisplayName = username
                     }


twitchAPI :: (MonadIO m, MonadReader TwitchCfg m, FromJSON a) => Text -> Text -> m (Response (TwitchResp a))
twitchAPI apiKind idParam = do
  cfg <- ask
  let apiUrl   = twitchcfg_endpoint cfg <> apiKind
      clientId = twitchcfg_clientid cfg
      url  :: String
      url   = printf "%s?id=%s" apiUrl idParam
      opts  = defaults & header "Client-ID" .~ [ encodeUtf8 clientId ]
  liftIO $ asJSON =<< getWith opts url


getVideoComment :: (MonadIO m, MonadReader TwitchCfg m) => Text -> m [Comment]
getVideoComment vodId = do
  cfg <- ask
  let clientId = twitchcfg_clientid cfg
      url  :: String
      url   = printf "https://api.twitch.tv/v5/videos/%s/comments" vodId
      opts  = defaults & header "Client-ID" .~ [ encodeUtf8 clientId ]
                       & header "Content-Type" .~ [ "application/vnd.twitchtv.v5+json" ]
  r <- liftIO $ asJSON =<< getWith opts url
  return $  commentresp_data (r ^. responseBody)


getVideoCommentOfUser :: (MonadIO m, MonadReader TwitchCfg m) => Text -> Text -> m [Text]
getVideoCommentOfUser vodId name = do
  comments <- getVideoComment vodId
  return $ comments ^.. folded
                      . filtered ((== name) . (view (comment_commenter . commenter_name)))
                      . comment_message
                      . message_body


extractBaseUrl :: Text -> Text
extractBaseUrl url = split (== '/' ) url !! 4

-- {
--       "_id": "64043f37-ca25-4730-88e0-4204b1570bc8",
--       "created_at": "2018-07-29T16:06:30.962Z",
--       "updated_at": "2018-07-29T16:06:30.962Z",
--       "channel_id": "94814458",
--       "content_type": "video",
--       "content_id": "291249101",
--       "content_offset_seconds": 24.162,
--       "commenter": {
--         "display_name": "ekmett",
--         "_id": "94814458",
--         "name": "ekmett",
--         "type": "user",
--         "bio": "I write a lot of Haskell.",
--         "created_at": "2015-06-29T17:37:50.497754Z",
--         "updated_at": "2018-08-01T20:33:32.049696Z",
--         "logo": "https://static-cdn.jtvnw.net/jtv_user_pictures/2a523ddf-518c-4d49-b793-a1acb98b901f-profile_image-300x300.png"
--       },
--       "source": "chat",
--       "state": "published",
--       "message": {
--         "body": "ok, might as well get started!",
--         "fragments": [
--           {
--             "text": "ok, might as well get started!"
--           }
--         ],
--         "is_action": false,
--         "user_badges": [
--           {
--             "_id": "broadcaster",
--             "version": "1"
--           },
--           {
--             "_id": "subscriber",
--             "version": "0"
--           },
--           {
--             "_id": "premium",
--             "version": "1"
--           }
--         ],
--         "user_color": "#FF0000"
--       },
--       "more_replies": false
--     }
