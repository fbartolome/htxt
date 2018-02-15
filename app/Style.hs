module Style
(   theMap,
    tiltOn
) where

import Brick
import Graphics.Vty

-- Attribute names

tiltOn :: AttrName
tiltOn = attrName "tiltOn"

-- Map

theMap :: AttrMap
theMap = attrMap defAttr [(tiltOn, bg magenta)]