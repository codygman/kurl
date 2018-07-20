{-# language OverloadedStrings #-}
{-# language AllowAmbiguousTypes #-}
{-# language FlexibleContexts #-}


module Main where


import           Control.Monad
import           Control.Lens
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as LB
import           Data.ByteString.Char8     (pack)
import           Data.Function             ((&))
import           Network.Wreq
import           Streamly
import qualified Streamly.Prelude       as S


main :: IO ()
main = do
  putStrLn "start downloading..."

  let baseUrl = "http://fastly.vod.hls.ttvnw.net"
      path    = "/ff5ed1e62db80d5708d4_amazhs_29515275280_913374868/chunked/"
      port    = "80"

  runStream . serially $ do

    let urls = fmap (\ts -> baseUrl ++ ":" ++ port ++ path ++ show ts ++ ".ts")
                    [0..460]
               -- [ "http://localhost/1.ts"
               -- , "http://localhost/2.ts"
               -- , "http://localhost/3.ts"
               -- ]
    S.fromFoldableM $ fmap appendTs urls


fetchTs :: String -> IO B.ByteString
fetchTs url = do
  putStrLn $ "download => " ++ show url
  response <- get url
  return . LB.toStrict $ response ^. responseBody


writeBytes :: B.ByteString -> IO ()
writeBytes bs = do
  B.appendFile "all.ts" bs


appendTs :: String -> IO ()
appendTs url = fetchTs url >>= writeBytes


concatBytes :: B.ByteString -> B.ByteString -> IO B.ByteString
concatBytes a b = B.append <*> return a <$> return b
