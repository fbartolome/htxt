module Data.Styled.StyleChar
  ( StyleChar(..)
  , Attrs(..)
  , charWnoAttrs
  , styleCharWnoAttrs
  , hasAttrs
  , stringToStyleChars
  , setSelection
  , setSearch
  , renderChar
  , toString
  ) where

import qualified Brick             as B
import qualified Widget.UIResource as UI

import qualified Data.Styled.Style as S

data Attrs = Attrs
  { selected      :: Bool
  , searched      :: Bool
  , lineIndicator :: Bool
  }

data StyleChar = StyleChar
  { char  :: Char
  , attrs :: Attrs
  }

instance Eq StyleChar where
  sc1 == sc2 = (char sc1) == (char sc2)

charWnoAttrs :: Char -> StyleChar
charWnoAttrs c = StyleChar c (Attrs False False False)

styleCharWnoAttrs :: StyleChar -> StyleChar
styleCharWnoAttrs (StyleChar c _) = StyleChar c (Attrs False False False)

stringToStyleChars :: Attrs -> String -> [StyleChar]
stringToStyleChars attrs str = map (\ch -> StyleChar ch attrs) str

hasAttrs :: StyleChar -> Bool
hasAttrs (StyleChar _ (Attrs False False False)) = False
hasAttrs sc                                      = True

setSelection :: Bool -> StyleChar -> StyleChar
setSelection b (StyleChar c attrs) = StyleChar c $ attrs {selected = b}

setSearch :: Bool -> StyleChar -> StyleChar
setSearch b (StyleChar c attrs) = StyleChar c $ attrs {searched = b}

renderChar :: StyleChar -> B.Widget UI.UIResource
renderChar (StyleChar c (Attrs sel sea li))
  | li = B.withAttr S.lineIndicator $ B.str [c]
  | sel && sea = B.withAttr S.searchAndSelected $ B.str [c]
  | sel && not sea = B.withAttr S.selected $ B.str [c]
  | not sel && sea = B.withAttr S.search $ B.str [c]
  | otherwise = B.str [c]

toString :: [StyleChar] -> String
toString scs = map (\x -> char x) scs
