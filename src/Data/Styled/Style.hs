module Data.Styled.Style
  ( theMap
  , lineIndicator
  , selected
  , search
  , searchAndSelected
  ) where

import           Brick
import           Graphics.Vty

-- Attribute names
lineIndicator :: AttrName
lineIndicator = attrName "lineIndicator"

selected :: AttrName
selected = attrName "selected"

search :: AttrName
search = attrName "search"

searchAndSelected :: AttrName
searchAndSelected = attrName "searchAndSelected"

-- Map
theMap :: AttrMap
theMap =
  attrMap
    defAttr
    [ (lineIndicator, fg brightBlack)
    , (selected, bg magenta)
    , (search, black `on` yellow)
    , (searchAndSelected, black `on` brightYellow)
    ]
