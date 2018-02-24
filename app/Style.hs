module Style
(   theMap,
    tiltOn
) where

import Brick
import Graphics.Vty

-- Attribute names

tiltOn :: AttrName
tiltOn = attrName "tiltOn"

search :: AttrName
search = attrName "search"

-- Map

theMap :: AttrMap
theMap = attrMap defAttr [(tiltOn, bg magenta), (search, black `on` yellow)]
