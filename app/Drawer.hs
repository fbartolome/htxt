module Drawer
  ( drawUI
  )
where

import qualified Brick                      as B
import           Brick.Widgets.Border
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
import           Data.List.Split
import           Data.Maybe
import           Data.Tuple

import           Application
import           Cursor
import           State
import           Style
import           StyleChar

drawUI :: State -> [B.Widget UIResource]
drawUI s = [drawBoxes s]

drawBoxes :: State -> B.Widget UIResource
drawBoxes s = B.vBox [drawTextBox s, drawBottomBox s]

drawTextBox :: State -> B.Widget UIResource
drawTextBox s = borderWithLabel (B.str $ " " ++ getFilename s ++ " ")
  $ B.viewport EditorViewpoint B.Both
  $ B.showCursor EditorCursor (B.Location (swap $ getCurrentPosition $ text s))
  $ B.visibleRegion (B.Location (swap $ getCurrentPosition $ text s)) (1, 1)
  $ B.vBox $ drawText $ adaptText s

drawText :: [[StyleChar]] -> [B.Widget UIResource]
drawText = map (\x -> drawLine x)

drawLine :: [StyleChar] -> B.Widget UIResource
drawLine ss = B.hBox (foldr (\c h -> (drawChar c):h) [B.str " "] ss) where
    drawChar sc
        | style sc == Nothing = B.str [char sc]
        | otherwise = B.withAttr (fromJust $ style sc) $ B.str [char sc]

adaptText :: State -> [[StyleChar]]
adaptText s = adaptSize (getLines $ text s) $ terminalSize s

adaptSize :: [[StyleChar]] -> (Int, Int) -> [[StyleChar]]
adaptSize [[]] _ = [[StyleChar ' ' Nothing]]
adaptSize s (rows, cols) = foldr (\line h -> (changeEmptyLine (chunksOf (rows-2) line)) ++ h) [] s where
  changeEmptyLine [] = [[StyleChar ' ' Nothing]]
  changeEmptyLine l  = l

-- Bottom box

drawBottomBox :: State -> B.Widget UIResource
drawBottomBox s =
  B.vLimit 3
  $ border
  $ B.padRight (B.Max)
  $ bottomBoxText $ mode s

bottomBoxText :: Mode -> B.Widget UIResource
bottomBoxText Insert = C.center $ drawLine $ auxiliarText Insert
bottomBoxText (Search s d) = drawLine $ auxiliarText (Search s d)
