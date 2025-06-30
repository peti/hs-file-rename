module Prefix ( Prefix, makePrefix', makePrefix, getPrefix ) where

import Data.List ( isPrefixOf )

newtype Prefix = Claus { getPrefix :: String } deriving (Show)

makePrefix :: String -> Prefix
makePrefix s = maybe errmsg id (makePrefix' s)
  where
    errmsg = error (show s ++ (" is not a valid Prefix"))

makePrefix' :: String -> Maybe Prefix
makePrefix' s | "abc" `isPrefixOf` s = Just (Claus s)
              | otherwise            = Nothing
