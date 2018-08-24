{-# LANGUAGE OverloadedStrings   #-}


module TsIO
  ( processM3U8
  , processTS
  , writeComments
  ) where


import           Control.Lens                 ((^.))
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as LB
import           Data.Text                    (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TI
import           Network.Wreq                 (get, responseBody)
import           Text.Printf                  (printf)


processTS :: Text -> Int -> IO ()
processTS vodBaseUrl tsIdx = do
  let tsName  = T.pack $ show tsIdx <> ".ts"
  processFile vodBaseUrl tsName


processM3U8 :: Text -> Text -> IO ()
processM3U8 vodBaseUrl m3u8Filename = do
  printf "downloading => %s as local file %s\n" vodBaseUrl m3u8Filename
  processFile vodBaseUrl m3u8Filename


processFile :: Text -> Text -> IO ()
processFile fullUrl tsName  = do
  fetchTs fullUrl >>= writeTs tsName


fetchTs :: Text ->  IO B.ByteString
fetchTs url = do
  printf "download => %s\n" url
  response <- get . T.unpack $ url
  return . LB.toStrict $ response ^. responseBody


writeTs :: Text -> B.ByteString -> IO ()
writeTs filename  bs = do
  printf "writing => %s\n"  filename
  B.writeFile (T.unpack filename) bs


writeComments :: String -> Text -> [(Text, Text, Text)] -> IO ()
writeComments vodId vodUserName ts = do
  let filename :: String
      filename = printf "%s_comments_%s.txt" vodUserName vodId
  printf "writing => %s\n"  filename
  TI.writeFile filename (foldMap destructTup ts)
  where
    destructTup :: (Text, Text, Text) -> Text
    destructTup (a,b,c) = T.pack $ printf "%-24s, %-15s, \"%s\"\n" a b c
