module Data.File where

import qualified Data.Text as T

data File = File
  { fileName :: String
  , filePath :: String
  }
