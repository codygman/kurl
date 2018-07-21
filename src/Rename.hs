module Rename
  ( vodRespMapper
  ) where

vodRespMapper :: String -> String
vodRespMapper "vodtype" = "type"
vodRespMapper name = name
