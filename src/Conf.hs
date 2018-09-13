{-# LANGUAGE DeriveGeneric   #-}

module Conf
  ( KurlConf(..)
  ) where


import qualified Data.Text as T
import qualified Dhall as Dh

data KurlConf = KurlConf
  { kurlConfClientId :: T.Text
  , kurlConfLive     :: Bool
  , kurlConfBare     :: Bool
  } deriving (Dh.Generic, Show)

instance Dh.Interpret KurlConf

