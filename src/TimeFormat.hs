{-# LANGUAGE RankNTypes #-}


module TimeFormat
  ( format4file
  , format4ffmpeg
  , formatUtc
  , makeOffset
  ) where


import Data.List                     (isPrefixOf)
import Data.Time                     (UTCTime, NominalDiffTime, formatTime, defaultTimeLocale)
import Text.ParserCombinators.Parsec
import Text.Printf                   (printf)


-- parseDuration :: Maybe String -> Maybe UTCTime
-- parseDuration mx = case mx of
--   Nothing -> Nothing
--   Just x ->  parseRangeTime "%-dd%-Hh%-Mm%-Ss" x
--               <|> parseRangeTime "%-Hh%-Mm%-Ss" x
--               <|> parseRangeTime "%-Mm%-Ss" x
--               <|> parseRangeTime "%-Mm%-Ss" x
--               <|> parseRangeTime "%-Hh%-Mm" x
--               <|> parseRangeTime "%-Hh" x
--               <|> parseRangeTime "%-Mm" x
--               <|> parseRangeTime "%-Ss" x
--   where
--     parseRangeTime timeFormat x = parseTimeM True defaultTimeLocale timeFormat x


format4file :: NominalDiffTime -> String
format4file = formatNominalDiff "%dh%dm%ds"


format4ffmpeg :: NominalDiffTime -> String
format4ffmpeg = formatNominalDiff "%d:%d:%d"


formatNominalDiff :: String -> NominalDiffTime -> String
formatNominalDiff fmt diff =
  let totalSec = div (fromEnum diff) (10^12)
      (hour, hsec) = divMod totalSec 3600 :: (Int, Int)
      (min, sec)   = divMod hsec 60 :: (Int, Int)
  in printf fmt hour min sec


formatUtc :: UTCTime -> String
formatUtc = formatTime defaultTimeLocale "%_C%y-%m-%d_%Hh%Mm%Ss"


makeOffset :: String -> NominalDiffTime
makeOffset str =
  case parse (try (ffmpegDurationP) <|> twitchDurationP) "" str of
    Left e -> error "offset input error"
    Right totalsec -> toEnum (totalsec * 10^12)


ffmpegDurationP :: Parser Int
ffmpegDurationP = do
  h <- read <$> many1 digit <* char ':'
  m <- read <$> digit1or2   <* char ':'
  s <- read <$> digit1or2   <* eof
  return $ h * 3600 + m * 60 + s


twitchDurationP :: Parser Int
twitchDurationP = try hhmmss
              <|> try hhmm
              <|> try hhss
              <|> try mmss
              <|> try hh
              <|> try mm
              <|> ss


-- parses only 00 to 59
-- 00 to 09 can be represented in the form of single digit 0 to 9
digit1or2 :: Parser String
digit1or2  = try (consChar <$> sixRadix <*> digit)
             <|> (char2Str <$> digit)
  where
    nullChar     = 'n'
    consChar a b = [a,b]
    char2Str a   = [a]
    sixRadix     = oneOf "012345"


hhmmss :: Parser Int
hhmmss = do
  h <- read <$> many1 digit <* char 'h'
  m <- read <$> digit1or2   <* char 'm'
  s <- read <$> digit1or2   <* char 's'
  return $ h * 3600 + m * 60 + s


hhmm :: Parser Int
hhmm = do
  h <- read <$> many1 digit <* char 'h'
  m <- read <$> digit1or2   <* char 'm'
  return $ h * 3600 + m * 60


mmss :: Parser Int
mmss = do
  m <- read <$> many1 digit <* char 'm'
  s <- read <$> digit1or2   <* char 's'
  return $ m * 60 + s


hhss :: Parser Int
hhss = do
  h <- read <$> many1 digit <* char 'h'
  s <- read <$> digit1or2   <* char 's'
  return $ h * 3600 + s


hh :: Parser Int
hh = do
  h <- read <$> many1 digit <* char 'h'
  return $ h * 3600


mm :: Parser Int
mm = do
  m <- read <$> many1 digit <* char 'm'
  return $ m * 60


ss :: Parser Int
ss = do
  s <- read <$> many1 digit <* char 's'
  return s
