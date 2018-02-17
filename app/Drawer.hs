module Drawer
(   drawUI
) where

import Data.Maybe
import Data.List.Split
import qualified Brick as B
import Brick.Widgets.Border
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border.Style as BS
import Cursor
import State
import Style

drawUI :: State -> [B.Widget ()]
drawUI s = [drawTextBox s]

drawTextBox :: State -> B.Widget ()
drawTextBox s =  B.withBorderStyle BS.defaultBorderStyle
  $ borderWithLabel (B.str $ " " ++ getFilename s ++ " ")
  $ B.padBottom (B.Max)
  $ B.padRight (B.Max)
  $ B.vBox $ drawText $ adaptText s

drawText :: [[StyleChar]] -> [B.Widget ()]
drawText = map (\x -> B.hBox $ drawLine x)

drawLine :: [StyleChar] -> [B.Widget ()]
drawLine = foldr (\c h -> (drawChar c):h) [B.str " "] where
    drawChar sc
        | style sc == Nothing = B.str [char sc]
        | otherwise = B.withAttr (fromJust $ style sc) $ B.str [char sc]

adaptText :: State -> [[StyleChar]]
adaptText s = adaptSize (getLines $ text s) $ terminalSize s

adaptSize :: [[StyleChar]] -> (Int, Int) -> [[StyleChar]]
adaptSize [[]] _ = [[StyleChar ' ' Nothing]]
adaptSize s (rows, cols) = foldr (\line h -> (changeEmptyLine (chunksOf (rows-2) line)) ++ h) [] s where
  changeEmptyLine [] = [[StyleChar ' ' Nothing]]
  changeEmptyLine l = l
