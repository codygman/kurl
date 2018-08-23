{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}


module M3u8
  ( parseM3u8
  , StreamInfo(..)
  , streaminfo_quality
  , streaminfo_url
  ) where


import Data.Map.Strict
import Data.Map.Strict as M (lookup)
import Data.Text
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec as P (noneOf)
import Data.Maybe
import Data.Either
import Control.Lens


data StreamInfo = StreamInfo
  { _streaminfo_quality :: String
  , _streaminfo_url     :: String
  } deriving (Show)

makeLenses ''StreamInfo


parseM3u8 :: String -> [StreamInfo]
parseM3u8 m3u8 = case parse m3uParser "" m3u8 of
                   Left e   -> error $ show e
                   Right ls -> ls


m3uParser :: Parser [StreamInfo]
m3uParser = do
  extm3u         *> newline
  extxtwitchinfo *> newline
  ls <- many (entry <* newline)
  return ls


entry :: Parser StreamInfo
entry = do
  extxmedia
  infMap <- newline *> extxstreaminf
  url    <- newline *> urlbase64
  let quality = fromMaybe "unknown" (M.lookup "VIDEO" infMap)
  return $ StreamInfo quality url


urlbase64 :: Parser String
urlbase64 = do
  protocol <- string "https://"
  host <- many (P.noneOf "/")
  path <- many (char '/' <|> base64)
  ext  <- string ".m3u8"
  return $ protocol <> host <> path <> ext
  where
    base64 = alphaNum <|> char '-' <|> char '_'


extm3u :: Parser ()
extm3u = string "#EXTM3U" *> return ()


extxtwitchinfo :: Parser (Map String String)
extxtwitchinfo = do
  string "#EXT-X-TWITCH-INFO:"
  kv <- sepBy keyValue (char ',')
  return $ fromList kv


extxmedia :: Parser (Map String String)
extxmedia = do
  string "#EXT-X-MEDIA:"
  kv <- sepBy keyValue (char ',')
  return $ fromList kv


extxstreaminf :: Parser (Map String String)
extxstreaminf = do
  string "#EXT-X-STREAM-INF:"
  kv <- sepBy keyValue (char ',')
  return $ fromList kv


keyValue :: Parser (String, String)
keyValue = do
  k <- many (alphaNum <|> char '-')
  char '='
  v <- (char '\"' >>  manyTill anyChar (try (char '\"')))
       <|> many (P.noneOf ",\n")
  return (k, v)


-- streamQuality :: Parser String
-- streamQuality =
--   string "chunked"
--   <|> string "720p60"
--   <|> string "720p30"
--   <|> string "480p30"
--   <|> string "360p30"
--   <|> string "160p30"
--   <|> string "audio_only"
