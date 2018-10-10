{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# OPTIONS -Wno-unused-top-binds #-}


module Twitch
  ( getLive
  , getArchive
  , getChatLogs
  , getLiveStreamList
  , isHosting
  , VideoInfo(..)
  , StreamType(..)
  , TwitchCfg(..)
  ) where


import           Control.Exception                 ( try, SomeException(..))
import           Control.Lens
import           Control.Monad                     (foldM)
import           Control.Monad.Reader              (MonadIO, MonadReader, runReaderT, reader, liftIO)
import           Data.Aeson                        (fieldLabelModifier, defaultOptions, FromJSON)
import           Data.Aeson.TH                     (deriveJSON)
import qualified Data.ByteString.Char8      as CB  (pack)
import qualified Data.ByteString.Lazy.Char8 as LB  (unpack)
import           Data.List.Split            as LS  (chunksOf)
import           Data.Maybe                        (fromMaybe, isNothing, maybe)
import           Data.Text                  hiding (drop)
import           Data.Text                  as T   (length, intercalate)
import           Data.Text.Encoding         as E   (encodeUtf8, decodeUtf8)
import           Data.Time                         (NominalDiffTime)
import           GHC.Generics                      (Generic)
import           Network.Wreq                      (responseBody, defaults, header, param, asJSON, getWith, get)
import           Text.Printf                       (printf)
import           System.Random                     (getStdRandom, randomR)

import           M3u8                              (StreamInfo, streaminfo_quality, streaminfo_url, parseM3u8)


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
  , _comment_updated_at             :: Text
  , _comment_channel_id             :: Text
  , _comment_content_type           :: Text
  , _comment_content_id             :: Text
  , _comment_content_offset_seconds :: !Float
  , _comment_commenter              :: !Commenter
  , _comment_source                 :: Text
  , _comment_state                  :: Text
  , _comment_message                :: !Message
  , _comment_more_replies           :: Bool
  } deriving (Show, Generic)


data Commenter = Commenter
  { _commenter_display_name :: !Text
  , _commenter__id          :: !Text
  , _commenter_name         :: !Text
  , _commenter_type         :: Text
  , _commenter_bio          :: Maybe Text
  , _commenter_created_at   :: Text
  , _commenter_updated_at   :: Text
  , _commenter_logo         :: Maybe Text
  } deriving (Show, Generic)


data Message = Message
  { _message_body        :: !Text
  , _message_fragments   :: [Fragment]
  , _message_is_action   :: Bool
  , _message_user_badges :: Maybe [UserBadge]
  , _message_user_color  :: Maybe Text
  } deriving (Show, Generic)


data FollowData = FollowData
  { _follow_total      :: !Int
  , _follow_data       :: [FollowEntry]
  , _follow_pagination :: Pagination
  } deriving (Show, Generic)


data FollowEntry = FollowEntry
  { _followEntry_from_id     :: !Text
  , _followEntry_to_id       :: !Text
  , _followEntry_followed_at :: !Text
  } deriving (Show, Generic)


data StreamData = StreamData
  { _stream_data       :: [StreamEntry]
  , _stream_pagination :: Pagination
  } deriving (Show, Generic)


newtype Pagination = Pagination
  { _pagination_cursor :: Maybe Text
  } deriving (Show, Generic)


data StreamEntry = StreamEntry
  { _streamEntry_id            :: Text
  , _streamEntry_user_id       :: !Text
  , _streamEntry_game_id       :: Text
  , _streamEntry_community_ids :: [Text]
  , _streamEntry_type          :: !Text
  , _streamEntry_title         :: Text
  , _streamEntry_viewer_count  :: Int
  , _streamEntry_started_at    :: Text
  , _streamEntry_language      :: Text
  , _streamEntry_thumbnail_url :: Text
  } deriving (Show, Generic)
-- data Channel = Channel
--   { _channel__id                             :: !Int
--   , _channel_background                      :: Text
--   , _channel_banner                          :: Text
--   , _channel_broadcaster_language            :: Text
--   , _channel_created_at                      :: Text
--   , _channel_delay                           :: Text
--   , _channel_display_name                    :: !Text
--   , _channel_followers                       :: Int
--   , _channel_game                            :: !Text
--   , _channel_language                        :: Text
--   , _channel_logo                            :: Text
--   , _channel_mature                          :: Bool
--   , _channel_name                            :: !Text
--   , _channel_partner                         :: Bool
--   , _channel_profile_banner                  :: Text
--   , _channel_profile_banner_background_color :: Text
--   , _channel_status                          :: !Text
--   , _channel_updated_at                      :: Text
--   , _channel_url                             :: !Text
--   , _channel_video_banner                    :: Text 
--   , _channel_views                           :: !Int
--   } deriving (Show, Generic)


data HostingData = HostingData
  { _hosting_hosts :: [HostingEntry]
  } deriving (Show, Generic)


data HostingEntry = HostingEntry
  { _hostingEntry_host_id             :: Int
  , _hostingEntry_host_login          :: Text
  , _hostingEntry_host_display_name   :: Text
  , _hostingEntry_target_id           :: Maybe Int
  , _hostingEntry_target_login        :: Maybe Text
  , _hostingEntry_target_display_name :: Maybe Text
  } deriving (Show, Generic)


newtype Fragment = Fragment
  { _fragment_text :: Text
  } deriving (Show, Generic)


data UserBadge = UserBadge
  { _userbadge__id     :: Text
  , _userbadge_version :: Text
  } deriving (Show, Generic)


data AccessToken = AccessToken
  { _accesstoken_sig   :: Text
  , _accesstoken_token :: Text
  } deriving (Show, Generic)


deriveJSON defaultOptions { fieldLabelModifier = const "data"                    } ''TwitchData
deriveJSON defaultOptions { fieldLabelModifier = const "comments"                } ''CommentData
deriveJSON defaultOptions { fieldLabelModifier = drop (T.length "_video_")       } ''Video
deriveJSON defaultOptions { fieldLabelModifier = drop (T.length "_user_")        } ''User
deriveJSON defaultOptions { fieldLabelModifier = drop (T.length "_comment_")     } ''Comment
deriveJSON defaultOptions { fieldLabelModifier = drop (T.length "_commenter_")   } ''Commenter
deriveJSON defaultOptions { fieldLabelModifier = drop (T.length "_message_")     } ''Message
deriveJSON defaultOptions { fieldLabelModifier = drop (T.length "_fragment_")    } ''Fragment
deriveJSON defaultOptions { fieldLabelModifier = drop (T.length "_userbadge_")   } ''UserBadge
deriveJSON defaultOptions { fieldLabelModifier = drop (T.length "_accesstoken_") } ''AccessToken
deriveJSON defaultOptions { fieldLabelModifier = drop (T.length "_follow_")      } ''FollowData
deriveJSON defaultOptions { fieldLabelModifier = drop (T.length "_followEntry_") } ''FollowEntry
deriveJSON defaultOptions { fieldLabelModifier = drop (T.length "_pagination_")  } ''Pagination
-- deriveJSON defaultOptions { fieldLabelModifier = drop (T.length "_channel_")     } ''Channel
deriveJSON defaultOptions { fieldLabelModifier = drop (T.length "_stream_")      } ''StreamData
deriveJSON defaultOptions { fieldLabelModifier = drop (T.length "_streamEntry_") } ''StreamEntry
deriveJSON defaultOptions { fieldLabelModifier = drop (T.length "_hosting_")      } ''HostingData
deriveJSON defaultOptions { fieldLabelModifier = drop (T.length "_hostingEntry_") } ''HostingEntry


makeLenses ''TwitchData
makeLenses ''Video
makeLenses ''User
makeLenses ''CommentData
makeLenses ''Comment
makeLenses ''Commenter
makeLenses ''Message
makeLenses ''Fragment
makeLenses ''UserBadge
makeLenses ''AccessToken
makeLenses ''FollowData
makeLenses ''FollowEntry
makeLenses ''Pagination
-- makeLenses ''Channel
makeLenses ''StreamData
makeLenses ''StreamEntry
makeLenses ''HostingData
makeLenses ''HostingEntry


data VideoInfo = VideoInfo
  { videoinfo_baseUrl         :: Text
  , videoinfo_userDisplayName :: Text
  , videoinfo_duration        :: Maybe Text
  } deriving (Show)


newtype TwitchCfg = TwitchCfg
  { twitchcfg_clientid  :: Text
  }

type TwitchMonad m = (Monad m, MonadReader TwitchCfg m, MonadIO m)


data ApiKind = Videos | Users | Follows | Login | Streams


data StreamType = Live | Archive
  deriving (Show)


-- TODO: Remove hardcoded clientId. Maybe we can use Dhall as configuration.
--       Do we even need Reader pattern here?
-- TODO: Rate limit problem. for now remove hardcoded client for preventing rate limit.
--       and add new documentation about getting twitch clientID

twitchCfg :: TwitchCfg
twitchCfg = TwitchCfg "Insert your Client-ID here"  -- cliendId


getLive :: String -> String -> IO VideoInfo
getLive quality channelName = flip runReaderT twitchCfg $ do
  fullUrl <- m3u8Url Live quality channelName
  return $ VideoInfo (fullUrl ^. streaminfo_url . to pack) (pack channelName) Nothing


getArchive :: String -> String -> IO VideoInfo
getArchive quality vodId = flip runReaderT twitchCfg $ do
  fullUrl <- m3u8Url Archive quality vodId
  videos  <- twitchAPI Videos Nothing [(E.decodeUtf8 . CB.pack $ vodId)]
  let userId   = videos ^?! twitch_data . traverse . video_user_id
      duration = videos ^?! twitch_data . traverse . video_duration
  users <- twitchAPI Users Nothing [userId]
  let username =  users ^?! twitch_data . traverse . user_display_name
  return $ VideoInfo (fullUrl ^. streaminfo_url . to pack) username (Just duration)


twitchAPI :: (TwitchMonad m, FromJSON a) => ApiKind -> Maybe Text -> [Text] -> m a
twitchAPI apiKind cursor idParams = do
  clientId  <- reader twitchcfg_clientid
  let cursorParam = maybe "" (printf "&after=%s") cursor
      url         = printf newApiFmt (T.intercalate queryParam idParams) <> cursorParam
      opts        = defaults & header "Client-ID" .~ [ E.encodeUtf8 clientId ]
  -- liftIO $ printf "querying with => %s...\n" (Prelude.take 100 url)
  jsonResp <- liftIO $
    (try :: IO a -> IO (Either SomeException a)) (getWith opts url) >>= \case
    Left e     -> error $ "twitchAPI network IO error" ++ show e
    Right resp -> asJSON resp
  return $ jsonResp ^. responseBody
  where
    newApiFmt = case apiKind of
                  Videos  -> "https://api.twitch.tv/helix/videos?id=%s"
                  Users   -> "https://api.twitch.tv/helix/users?id=%s"
                  Follows -> "https://api.twitch.tv/helix/users/follows?from_id=%s"
                  Login   -> "https://api.twitch.tv/helix/users?login=%s"
                  Streams -> "https://api.twitch.tv/helix/streams?user_id=%s"
  -- for repeated query parameters eg) user_id=123?user_id=345
    queryParam = case apiKind of
                   Videos  -> "&id="
                   Users   -> "&id="
                   Follows -> "&from_id="
                   Login   -> "&login="
                   Streams -> "&user_id="


m3u8Url :: TwitchMonad m => StreamType -> String -> String -> m StreamInfo
m3u8Url streamType streamQuality target = do
  let  errFmt = "Twitch: There's no stream which has the type: %s and quality: %s"
  m3u8Entry target >>= \case
    Nothing   -> error $ printf errFmt (show streamType) streamQuality
    Just  url -> return url
  where
    m3u8Entry loginUserOrVodId = do
      accessToken <- getAccessToken streamType loginUserOrVodId
      m3u8        <- m3u8Content streamType loginUserOrVodId accessToken
      return $ findOf folded ((== streamQuality) . view streaminfo_quality) (parseM3u8 m3u8)


m3u8Content :: TwitchMonad m => StreamType -> String -> AccessToken -> m String
m3u8Content streamType loginUserOrVodId accessToken = do
  clientId  <- reader twitchcfg_clientid
  rnd <- liftIO $ getStdRandom (randomR (1, 99999 :: Int))
  let token     = accessToken ^. accesstoken_token
      sig       = accessToken ^. accesstoken_sig
      url       = printf m3u8Fmt loginUserOrVodId
      randomInt = printf "%d" rnd
      m3u8Opts  = defaults & header "Client-ID"        .~ [ E.encodeUtf8 clientId ]
                           & param  "player"           .~ [ "twitchweb"           ]
                           & param  "token"            .~ [ token                 ]
                           & param  "sig"              .~ [ sig                   ]
                           & param  "allow_audio_only" .~ [ "true"                ]
                           & param  "allow_source"     .~ [ "true"                ]
                           & param  "type"             .~ [ "any"                 ]
                           & param  "p"                .~ [ pack randomInt        ]
  resp <- liftIO $ getWith m3u8Opts url
  return $ resp ^. responseBody . to LB.unpack
  where
    archFmt = "https://usher.ttvnw.net/vod/%s.m3u8"
    liveFmt = "https://usher.ttvnw.net/api/channel/hls/%s.m3u8"
    m3u8Fmt = case streamType of
                Live    -> liveFmt
                Archive -> archFmt


getAccessToken :: TwitchMonad m => StreamType -> String -> m AccessToken
getAccessToken streamType loginUserOrVodId = do
  clientId <- reader twitchcfg_clientid
  let tokenOpts = defaults & header "Client-ID" .~ [ E.encodeUtf8 clientId ]
      tokenUrl  = printf tokenFmt loginUserOrVodId
  resp <- liftIO $ asJSON =<< getWith tokenOpts tokenUrl
  return $ resp ^. responseBody
  where
    archFmt  = "https://api.twitch.tv/api/vods/%s/access_token"
    liveFmt  = "https://api.twitch.tv/api/channels/%s/access_token"
    tokenFmt = case streamType of
                 Live    -> liveFmt
                 Archive -> archFmt


chatLogOffset :: TwitchMonad m => String -> Float -> m [Comment]
chatLogOffset vodId offset = do
  clientId <- reader twitchcfg_clientid
  let url  = printf v5ApiFmt vodId offset
      opts = defaults & header "Client-ID"    .~ [ E.encodeUtf8 clientId              ]
                      & header "Content-Type" .~ [ "application/vnd.twitchtv.v5+json" ]
  resp <- liftIO $ asJSON =<< getWith opts url
  return $ resp ^. responseBody . comment_data
  where
    v5ApiFmt = "https://api.twitch.tv/v5/videos/%s/comments?content_offset_seconds=%f"


getChatLogs :: String -> NominalDiffTime -> NominalDiffTime -> IO [(Text, Text, Text)]
getChatLogs vodId startNominalDiff endNominalDiff = flip runReaderT twitchCfg $ do
  comments <- untilM ssec
                (\csec nsec -> nsec == csec || nsec > esec)
                (\csec comments -> fromMaybe csec $ comments & lastOf (traverse . comment_content_offset_seconds))
                (\csec -> liftIO (printf "downloading chat comment from %f seconds\n" csec) >> chatLogOffset vodId csec)
  let time = Getter comment_created_at
      name = Getter $ comment_commenter . commenter_display_name
      mesg = Getter $ comment_message . message_body
  return $ comments ^.. traverse . runGetter ((,,) <$> time <*> name <*> mesg)
  where
    picoPrecision = 10**12
    ssec = (fromIntegral . fromEnum $ startNominalDiff) / picoPrecision
    esec = (fromIntegral . fromEnum $ endNominalDiff)   / picoPrecision

-- Doing monadic operation until predicate returns true
-- untilM initialValue predicate nextValue monadicAction
untilM :: forall m a b. (Monad m) => a -> (a -> a -> Bool) -> (a -> [b] -> a) -> (a -> m [b]) -> m [b]
untilM a predicate next mf = do
  c <- mf a
  let a' = next a c
  cs <- if predicate a a' then return mempty else mf a'
  return $ mappend cs c


getLiveStreamList :: Text -> IO [Text]
getLiveStreamList loginName = flip runReaderT twitchCfg $ do
   getFollowers loginName
     >>= getStreams
     >>= getUserLoginName


getFollowers :: (TwitchMonad m) => Text -> m [Text]
getFollowers loginName = do
  user <- twitchAPI Login Nothing [loginName]
  let userId = user ^?!  twitch_data . traverse . user_id
  getFollowers' userId Nothing []
  where
    getFollowers' uid cursor acc = do
      followers  <- twitchAPI Follows cursor [uid]
      let to_ids  = followers ^.. follow_data . traverse . followEntry_to_id
          acc'    = to_ids ++ acc
          cursor' = followers ^. follow_pagination . pagination_cursor
      if followers ^. follow_total <= Prelude.length acc'
        then return acc'
        else getFollowers' uid cursor' acc'


getStreams :: (TwitchMonad m) => [Text] -> m [Text]
getStreams ids = do
  -- TODO: we must consider api rate limit.
  foldM (\acc ids -> (acc ++) <$> getStreams' 1 Nothing [] ids) [] groupOfFollowIds
  where
    -- 30 is the rate limit of api calls per minute.
    apiCallRateLimit = 30
    -- 100 is the limit of multiple repeat of input parameter of user_id or user_login
    paramRepeatLimit = 100
    groupOfFollowIds = LS.chunksOf paramRepeatLimit ids
    getStreams' :: (TwitchMonad m) => Int -> Maybe Text -> [Text] -> [Text] -> m [Text]
    getStreams' n cursor acc ids = do
      -- liftIO $ printf "calling stream api #%d\n => %s" n (show acc)
      liveStreams <- twitchAPI Streams cursor ids
      let livestream_ids = liveStreams ^.. stream_data . traverse . streamEntry_user_id
          acc'    = livestream_ids  ++ acc
          cursor' = liveStreams ^. stream_pagination . pagination_cursor
      if isNothing cursor' || Prelude.null livestream_ids || n == apiCallRateLimit - 10
        then return acc'
        else getStreams' (n + 1) cursor' acc' ids



getUserLoginName :: (TwitchMonad m) => [Text] -> m [Text]
getUserLoginName ids = do
  foldM (\acc ids -> (acc ++) <$> getUserLoginName' ids) [] groupOfUserIds
  where
    paramRepeatLimit = 100
    groupOfUserIds = LS.chunksOf paramRepeatLimit ids
    getUserLoginName' :: (TwitchMonad m) => [Text] -> m [Text]
    getUserLoginName' ids' = do
      users <- twitchAPI Users Nothing ids'
      return $ users ^.. twitch_data . traverse . user_login



isHosting :: Text -> IO [HostingEntry]
isHosting loginName = flip runReaderT twitchCfg $ do
  users <- twitchAPI Login Nothing [loginName]
  let userId = users ^?! twitch_data . traverse . user_id
  hosting <- unsupportedTwitchAPI userId
  return $ hosting ^. hosting_hosts


-- api which is checking that stream hosting other channel is `unsupported feature` for now.
unsupportedTwitchAPI :: (TwitchMonad m, FromJSON a) => Text -> m a
unsupportedTwitchAPI idParam = do
  let url         = printf unsupportedApiFmt idParam
  -- liftIO $ printf "querying with => %s\n" url
  r <- liftIO $ asJSON =<< get url
  return $ r ^. responseBody
  where
    unsupportedApiFmt = "https://tmi.twitch.tv/hosts?include_logins=1&host=%s" 
