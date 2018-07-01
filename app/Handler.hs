module Handler
  ( UIEvent(..)
  , eventHandler
  ) where

import           Brick
import qualified Graphics.Vty as V
import           Data.Maybe

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
eventHandlerInsertMode s (VtyEvent (V.EvKey V.KBS []))            = continue $ modifyText deleteLeft $ pushUndo s
eventHandlerInsertMode s (VtyEvent (V.EvKey V.KEnter []))         = continue $ modifyText insertLine $ pushUndo s
eventHandlerInsertMode s (VtyEvent (V.EvKey (V.KChar c) []))      = continue $ modifyText (handleChar c) $ pushUndo s
eventHandlerInsertMode s (VtyEvent (V.EvKey V.KUp []))            = continue $ handleMoveUp s
eventHandlerInsertMode s (VtyEvent (V.EvKey V.KDown []))          = continue $ handleMoveDown s
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

handleMoveDown :: State -> State
handleMoveDown s
  | isJust (selection $ text s) = handleMoveDown $ modifyText moveRight s
  | rightOnCursor > terminalLength = foldr (\n h -> modifyText moveRight h) s [1..terminalLength]
  | length (down $ text s) == 0 = modifyText moveToLineEnd s
  | rightOnCursor > rightOnTerminal = modifyText moveToLineEnd s
  | otherwise = foldr (\n h -> modifyText moveRight h) (modifyText (moveDown . moveToLineStart) s) [1..movingCharsFromLeft]
  where terminalLength = ((fst $ terminalSize s)-2)
        rightOnCursor = (length $ right $ text s)
        leftOnTerminal = (length $ left $ text s) `mod` terminalLength
        rightOnTerminal = terminalLength - leftOnTerminal
        movingCharsFromLeft = min leftOnTerminal (length $ head $ down $ text s)

handleMoveUp :: State -> State
handleMoveUp s
  | isJust (selection $ text s) = handleMoveUp $ modifyText moveLeft s
  | leftOnCursor > terminalLength = foldr (\n h -> modifyText moveLeft h) s [1..terminalLength]
  | length (up $ text s) == 0 = modifyText moveToLineStart s
  | leftOnCursor > upLineTerminalLength = modifyText (moveToLineEnd . moveUp) s
  | otherwise = foldr (\n h -> modifyText moveRight h) (modifyText (moveUp . moveToLineStart) s) [1..movingCharsFromLeft]
  where terminalLength = ((fst $ terminalSize s)-2)
        leftOnCursor = (length $ left $ text s)
        leftOnTerminal = (length $ left $ text s) `mod` terminalLength
        upLineCursorLength = (length $ head $ up $ text s)
        upLineTerminalLength = upLineCursorLength `mod` terminalLength
        movingCharsFromLeft = (upLineCursorLength `quot` terminalLength) * terminalLength + leftOnTerminal
