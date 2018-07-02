module Handlers.SearchHandler
  ( eventHandlerSearchMode
  , search
  ) where

import           Brick
import qualified Graphics.Vty as V
import           Application

import qualified LineZipper as LZ
import Cursor
import qualified Style      as S
import StyleChar
import State

eventHandlerSearchMode :: State -> BrickEvent UIResource UIEvent -> EventM UIResource (Next State)
eventHandlerSearchMode s (VtyEvent (V.EvKey (V.KChar c) []))          = continue $ modifyScreenAndSearch (LZ.insert (StyleChar c Nothing)) s
eventHandlerSearchMode s (VtyEvent (V.EvKey (V.KBS) []))              = continue $ modifyScreenAndSearch LZ.delete s
eventHandlerSearchMode s (VtyEvent (V.EvKey V.KEnter []))             = continue $ moveToNextOccurrence s
eventHandlerSearchMode s (VtyEvent (V.EvKey (V.KLeft) []))            = continue $ modifySearch LZ.moveLeft s
eventHandlerSearchMode s (VtyEvent (V.EvKey (V.KRight) []))           = continue $ modifySearch LZ.moveRight s
eventHandlerSearchMode s (VtyEvent (V.EvKey (V.KLeft) [V.MShift]))    = continue $ modifySearch LZ.selectLeft s
eventHandlerSearchMode s (VtyEvent (V.EvKey (V.KRight) [V.MShift]))   = continue $ modifySearch LZ.selectRight s
eventHandlerSearchMode s (VtyEvent (V.EvKey (V.KLeft) [V.MCtrl]))     = continue $ modifySearch LZ.moveToStart s
eventHandlerSearchMode s (VtyEvent (V.EvKey (V.KRight) [V.MCtrl]))    = continue $ modifySearch LZ.moveToEnd s
-- Change Mode
-- TODO: ver si esta bien hacer el map selected/unselected
eventHandlerSearchMode s (VtyEvent (V.EvKey (V.KChar 'f') [V.MCtrl])) = continue $ changeMode Insert
                                                                      $ s {text = mapUnselected (\sc -> sc {style = Nothing})
                                                                                $ mapSelected (\sc -> sc {style = Just S.tiltOn})
                                                                                $ text s}
-- Default
eventHandlerSearchMode s _                                            = continue s

-- TODO: ver si esta bien hacer el map selected/unselected
modifyScreenAndSearch :: (LZ.LineZipper StyleChar -> LZ.LineZipper StyleChar) -> State -> State
modifyScreenAndSearch f s = search $ s' {text = mapUnselected (\sc -> sc {style = Nothing})
                                      $ mapSelected (\sc -> sc {style = Just S.tiltOn})
                                      $ text s}
                                      where
                                        s' = modifySearch f s

modifySearch :: (LZ.LineZipper StyleChar -> LZ.LineZipper StyleChar) -> State -> State
modifySearch f (State tx u r fi ts (Search lz p)) = State tx u r fi ts (Search (f lz) p)
modifySearch _ s                                  = s

search :: State -> State
search (State tx u r fi ts (Search lz p)) = State (moveToPosition searched p) u r fi ts (Search lz positions)
  where
    old                   = LZ.toList lz
    new                   = map (\(StyleChar c _) -> StyleChar c (Just S.search)) old
    p                     = getCurrentPosition tx
    (searched, positions) = searchAndReplace old new tx

moveToNextOccurrence :: State -> State
moveToNextOccurrence (State tx u r fi ts (Search lz (p:ps))) = (State (moveToPosition tx p) u r fi ts (Search lz (ps ++ [p])))
moveToNextOccurrence s = s
