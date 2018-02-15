module Drawer
(   drawUI
) where

import qualified Brick as B
import Brick.Widgets.Border
import Text.Wrap
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Core as WC
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.List as WL

import TextZipper
import State
import Style

drawUI :: State -> [B.Widget ()]
drawUI s = [drawTextBox s]

drawTextBox :: State -> B.Widget ()
drawTextBox s =  B.withBorderStyle BS.defaultBorderStyle
  $ borderWithLabel (B.str $ " " ++ getName s ++ " ") 
  $ B.padBottom (B.Max)
  $ B.padRight (B.Max)
  $ drawText $ toString $ text s

drawText :: String -> B.Widget ()
drawText s = B.str s