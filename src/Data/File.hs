module Data.File where

import           Data.List.Split
import qualified Data.Text       as T

data File = File
  { fileName :: String
  , filePath :: String
  }

makeFile :: String -> File
makeFile fp = File (getFile fp) fp
  where
    getFile path = last $ splitOn "/" path
