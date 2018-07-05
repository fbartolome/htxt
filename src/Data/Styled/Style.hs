module Data.Styled.Style
  ( theMap
  , selected
  , search
  , searchAndSelected
  ) where

import           Brick
import           Graphics.Vty

-- Attribute names
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
    [ (selected, bg magenta)
    , (search, black `on` yellow)
    , (searchAndSelected, black `on` brightYellow)
    ]
