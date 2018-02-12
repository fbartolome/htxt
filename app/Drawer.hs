module Drawer
(   drawUI
) where

import qualified Brick as B
import Brick.Widgets.Border
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Core as WC
import qualified Brick.Widgets.Border.Style as BS

import Text
import State

drawUI :: State -> [B.Widget ()]
drawUI s = [drawText s]

drawText :: State -> B.Widget ()
drawText s =  B.withBorderStyle BS.defaultBorderStyle
  $ borderWithLabel (B.str $ getName s) 
  $ B.padBottom (B.Max)
  $ B.padRight (B.Max)
  $ B.str $ toString $ text s