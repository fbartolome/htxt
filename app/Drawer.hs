module Drawer
(   drawUI
) where

import Data.Maybe
import qualified Brick as B
import Brick.Widgets.Border
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border.Style as BS
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
  $ B.vBox $ drawText $ toText $ text s

drawText :: Text -> [B.Widget ()]
drawText = map (\x -> B.hBox $ drawLine x)

drawLine :: Line -> [B.Widget ()]
drawLine = foldr (\c h -> (drawChar c):h) [B.str " "] where
    drawChar sc
        | style sc == Nothing = B.str [char sc]
        | otherwise = B.withAttr (fromJust $ style sc) $ B.str [char sc]

-- drawLine [] = [B.str " "]
-- drawLine l = map drawChar l where
--     drawChar sc
--         | style sc == Nothing = B.str [char sc]
--         | otherwise = B.withAttr (fromJust $ style sc) $ B.str [char sc]