module Handler
  ( UIEvent(..)
  , eventHandler
  ) where

import           Brick
import qualified Graphics.Vty as V

import           Application
import           Cursor
import           State
import           Style

-- TODO handle for Search mode
eventHandler :: State -> BrickEvent UIResource UIEvent -> EventM UIResource (Next State)
eventHandler s (VtyEvent (V.EvKey V.KEsc [])) = halt s
eventHandler s event
  | mode s == Insert                          = eventHandlerInsertMode s event
  | otherwise                                 = continue s

eventHandlerInsertMode :: State -> BrickEvent UIResource UIEvent -> EventM UIResource (Next State)
eventHandlerInsertMode s (VtyEvent (V.EvKey V.KBS []))            = continue $ modifyText delete $ pushUndo s
eventHandlerInsertMode s (VtyEvent (V.EvKey V.KEnter []))         = continue $ modifyText insertLine $ pushUndo s
eventHandlerInsertMode s (VtyEvent (V.EvKey (V.KChar c) []))      = continue $ modifyText (handleChar c) $ pushUndo s
eventHandlerInsertMode s (VtyEvent (V.EvKey V.KUp []))            = continue $ modifyText moveUp s
eventHandlerInsertMode s (VtyEvent (V.EvKey V.KDown []))          = continue $ modifyText moveDown s
eventHandlerInsertMode s (VtyEvent (V.EvKey V.KLeft []))          = continue $ modifyText moveLeft s
eventHandlerInsertMode s (VtyEvent (V.EvKey V.KLeft [V.MShift]))  = continue $ modifyText selectLeft s
eventHandlerInsertMode s (VtyEvent (V.EvKey V.KRight []))         = continue $ modifyText moveRight s
eventHandlerInsertMode s (VtyEvent (V.EvKey V.KRight [V.MShift])) = continue $ modifyText selectRight s
eventHandlerInsertMode s (VtyEvent (V.EvKey (V.KChar 'z') [V.MCtrl])) = continue $ undo s
eventHandlerInsertMode s (VtyEvent (V.EvKey (V.KChar 'x') [V.MCtrl])) = continue $ redo s
eventHandlerInsertMode s (VtyEvent (V.EvResize rows cols))        = continue $ resize s rows cols
eventHandlerInsertMode s (VtyEvent (V.EvKey V.KDown [V.MShift]))  = vScrollBy (viewportScroll EditorViewpoint) 1 >> continue s
eventHandlerInsertMode s (VtyEvent (V.EvKey V.KUp   [V.MShift]))  = vScrollBy (viewportScroll EditorViewpoint) (-1) >> continue s
eventHandlerInsertMode s (VtyEvent (V.EvKey V.KDown [V.MCtrl, V.MShift])) = continue $ modifyText moveLinesWithSelectionDown s
eventHandlerInsertMode s (VtyEvent (V.EvKey V.KUp   [V.MCtrl, V.MShift])) = continue $ modifyText moveLinesWithSelectionUp s
eventHandlerInsertMode s _                                        = continue s

modifyText :: (Cursor StyleChar -> Cursor StyleChar) -> State -> State
modifyText f s = s {text = mapUnselected (\sc -> sc {style = Nothing})
                           $ mapSelected (\sc -> sc {style = Just tiltOn})
                           $ f $ text s}

handleChar :: Char -> Cursor StyleChar -> Cursor StyleChar
handleChar ch c = insert (StyleChar ch Nothing) c

resize :: State -> Int -> Int -> State
resize s rows cols = s {terminalSize = (rows, cols)}
