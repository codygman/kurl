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
import           System.Environment

main :: IO ()
main = do
  [baseUrl, start, end] <- getArgs
  putStrLn "start downloading..."
  runStream . serially $ do
    let s = read start :: Int
        e = read end :: Int
        urls = fmap (\ts -> baseUrl ++ show ts ++ ".ts") [s .. e]
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
