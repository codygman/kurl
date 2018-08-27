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
  , VideoInfo(..)
  , TwitchCfg(..)
  , StreamType(..)
  ) where


import           Control.Lens
import           Control.Monad.Reader              (MonadIO, MonadReader, reader, liftIO)
import           Data.Aeson                        (fieldLabelModifier, defaultOptions, FromJSON)
import           Data.Aeson.TH                     (deriveJSON)
import qualified Data.ByteString.Char8      as CB  (pack)
import qualified Data.ByteString.Lazy.Char8 as LB  (unpack)
import           Data.Maybe                        (fromMaybe)
import           Data.Text                  hiding (drop)
import           Data.Text                  as T   (length)
import           Data.Text.Encoding         as E   (encodeUtf8, decodeUtf8)
import           Data.Time                         (NominalDiffTime)
import           GHC.Generics                      (Generic)
import           Network.Wreq                      (responseBody, defaults, header, param, asJSON, getWith)
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


deriveJSON defaultOptions { fieldLabelModifier = const "data"                  } ''TwitchData
deriveJSON defaultOptions { fieldLabelModifier = const "comments"              } ''CommentData
deriveJSON defaultOptions { fieldLabelModifier = drop (T.length "_video_")     } ''Video
deriveJSON defaultOptions { fieldLabelModifier = drop (T.length "_user_")      } ''User
deriveJSON defaultOptions { fieldLabelModifier = drop (T.length "_comment_")   } ''Comment
deriveJSON defaultOptions { fieldLabelModifier = drop (T.length "_commenter_") } ''Commenter
deriveJSON defaultOptions { fieldLabelModifier = drop (T.length "_message_")   } ''Message
deriveJSON defaultOptions { fieldLabelModifier = drop (T.length "_fragment_")  } ''Fragment
deriveJSON defaultOptions { fieldLabelModifier = drop (T.length "_userbadge_") } ''UserBadge
deriveJSON defaultOptions { fieldLabelModifier = drop (T.length "_accesstoken_") } ''AccessToken


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


data VideoInfo = VideoInfo
  { videoinfo_baseUrl         :: Text
  , videoinfo_userDisplayName :: Text
  , videoinfo_duration        :: Maybe Text
  } deriving (Show)


data TwitchCfg = TwitchCfg
  { twitchcfg_url_v5    :: Text
  , twitchcfg_url_new   :: Text
  , twitchcfg_clientid  :: Text
  , twitchcfg_chat_path :: Text
  }

type TwitchMonad m = (MonadIO m, MonadReader TwitchCfg m)


data StreamType = Live | Archive
  deriving (Show)


getArchive :: TwitchMonad m => String -> String -> m VideoInfo
getArchive quality vodId = do
  fullUrl <- m3u8Url Archive quality vodId
  videos  <- twitchAPI "videos" (E.decodeUtf8 . CB.pack $ vodId)
  let userId   = videos ^?! traverse . video_user_id
      duration = videos ^?! traverse . video_duration
  users <- twitchAPI "users" userId
  let username =  users ^?! traverse . user_display_name
  return $ VideoInfo (fullUrl ^. streaminfo_url . to pack) username (Just duration)
  where
    twitchAPI :: (TwitchMonad m, FromJSON a) => Text -> Text -> m [a]
    twitchAPI apiKind idParam = do
      newApiUrl <- reader twitchcfg_url_new
      clientId  <- reader twitchcfg_clientid
      let url  = printf "%s/%s?id=%s" newApiUrl apiKind idParam
          opts = defaults & header "Client-ID" .~ [ E.encodeUtf8 clientId ]
      r <- liftIO $ asJSON =<< getWith opts url
      return $ r ^. responseBody . twitch_data


getLive :: TwitchMonad m => String -> String -> m VideoInfo
getLive quality channelName = do
  fullUrl <- m3u8Url Live quality channelName
  return $ VideoInfo (fullUrl ^. streaminfo_url . to pack) (pack channelName) Nothing


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
      return $ findOf folded ( (== streamQuality) . (view streaminfo_quality)) (parseM3u8 m3u8)


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
  clientId  <- reader twitchcfg_clientid
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
  v5ApiUrl <- reader twitchcfg_url_v5
  chatPath <- reader twitchcfg_chat_path
  let url  = printf "%s/%s/%s=%f" v5ApiUrl vodId chatPath offset
      opts = defaults & header "Client-ID" .~ [ E.encodeUtf8 clientId ]
                          & header "Content-Type" .~ [ "application/vnd.twitchtv.v5+json" ]
  resp <- liftIO $ asJSON =<< getWith opts url
  return $ resp ^. responseBody . comment_data


getChatLogs :: TwitchMonad m => String -> NominalDiffTime -> NominalDiffTime -> m [(Text, Text, Text)]
getChatLogs vodId startNominalDiff endNominalDiff = do
  comments <- untilM ssec
                (\csec -> \nsec -> nsec == csec || nsec > esec)
                (\csec -> \comments -> fromMaybe csec $ comments & lastOf (traverse . comment_content_offset_seconds))
                (\csec -> (liftIO $ printf "downloading chat comment from %d seconds\n" csec) >> chatLogOffset vodId csec)
  let time = Getter $ comment_created_at
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
