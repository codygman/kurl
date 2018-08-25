{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}


module M3u8
  ( parseM3u8
  , getStartIdx
  , getEndIdx
  , streaminfo_quality
  , streaminfo_url
  , test
  , StreamInfo(..)
  ) where


import Control.Lens                       (makeLenses)
import Control.Monad.IO.Class             (liftIO)
import Data.Map.Strict                    (Map, fromList)
import Data.Map.Strict               as M (lookup)
import Data.Maybe                         (fromMaybe)
import Data.Time                          (NominalDiffTime)
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim                   (ParsecT, runParserT)
import Data.List                          (isPrefixOf)


data StreamInfo = StreamInfo
  { _streaminfo_quality :: String
  , _streaminfo_url     :: String
  } deriving (Show)


makeLenses ''StreamInfo


parseM3u8 :: String -> [StreamInfo]
parseM3u8 m3u8 = case parse m3uParser "" m3u8 of
                   Left e   -> error $ "M3u8: " <> show e
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
  host <- many (noneOf "/")
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
       <|> many (noneOf ",\n")
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

getStartIdx :: NominalDiffTime -> String -> Int
getStartIdx duration m3u8 = getIdx (>= duration) m3u8


getEndIdx :: NominalDiffTime -> String -> Int
getEndIdx duration m3u8 = getIdx (> duration) m3u8


getIdx :: (NominalDiffTime -> Bool) -> String -> Int
getIdx pred  m3u8 =
  case parse indexdvrp "" m3u8 of
    Left  e -> error $ "M3U8: " <> show e
    Right accumulatedDiffTimes -> length $ takeWhile pred accumulatedDiffTimes


indexdvrp :: Parser [NominalDiffTime]
indexdvrp = do
  extm3u
  newline *> string "#EXT-X-VERSION:"             <* skipMany (noneOf "\n")
  newline *> string "#EXT-X-TARGETDURATION:"      <* skipMany (noneOf "\n")
  newline *> string "#ID3-EQUIV-TDTG:"            <* skipMany (noneOf "\n")
  newline *> string "#EXT-X-PLAYLIST-TYPE:"       <* skipMany (noneOf "\n")
  newline *> string "#EXT-X-MEDIA-SEQUENCE:"      <* skipMany (noneOf "\n")
  newline *> string "#EXT-X-TWITCH-ELAPSED-SECS:" <* skipMany (noneOf "\n")
  newline *> string "#EXT-X-TWITCH-TOTAL-SECS:"   <* skipMany (noneOf "\n")
  nomDifftimes <- manyTill extinfNts (try (spaces *> extxendlist))
  return $ (scanl1 (+) nomDifftimes)
  where
    extxendlist = string "#EXT-X-ENDLIST"


extinfNts :: Parser NominalDiffTime
extinfNts = do
  floatStr <- newline *> extinf
  newline *> tsp
  return $ toNominalDiffTime floatStr
  where
    floatp = many1 (digit <|> char '.')
    extinf = string "#EXTINF:" *> floatp <* char ','
    tsp    = (++) <$> (many1 digit) <*> string ".ts"


toNominalDiffTime :: String -> NominalDiffTime
toNominalDiffTime s = let f = read s :: Float
                      in fromRational . toRational $ f


test = do
  content <- readFile "index-dvr.m3u8"
  case parse indexdvrp "" content of
    Left e -> error $ show e
    Right r -> return $ last r
