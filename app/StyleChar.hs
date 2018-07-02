module StyleChar
  ( StyleChar (..)
  , toNormal
  ) where

import           Brick

data StyleChar = StyleChar
  { char  :: Char
  , style :: Maybe AttrName
  }
  deriving (Show)

instance Eq StyleChar where
  sc1 == sc2 = (char sc1) == (char sc2)

toNormal :: [StyleChar] -> [StyleChar]
toNormal sc = map (\s -> s {style = Nothing}) sc
