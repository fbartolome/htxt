module Drawer
(   drawUI
) where

import qualified Brick as B

import Screen

drawUI :: ScreenZipper -> [B.Widget ()]
drawUI s = [B.str $ screenToString s]