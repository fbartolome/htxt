module Data.Styled.StyleChar
  ( StyleChar(..)
  , removeStyle
  ) where

import           Brick

data StyleChar = StyleChar
  { char  :: Char
  , style :: Maybe AttrName
  } deriving (Show)

instance Eq StyleChar where
  sc1 == sc2 = (char sc1) == (char sc2)

removeStyle :: [StyleChar] -> [StyleChar]
removeStyle cs = map (\c -> c {style = Nothing}) cs
