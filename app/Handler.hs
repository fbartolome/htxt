module Handler
  ( Name
  , Tick (..)
  , eventHandler
  ) where

import           Brick
import qualified Graphics.Vty as V

import           Cursor
import           State
import           Style

type Name = ()
data Tick = Tick

eventHandler :: State -> BrickEvent Name Tick -> EventM Name (Next State)
eventHandler s (VtyEvent (V.EvKey V.KBS [])) = continue $ modifyText s delete
eventHandler s (VtyEvent (V.EvKey V.KEnter [])) = continue $ modifyText s insertLine
eventHandler s (VtyEvent (V.EvKey (V.KChar c) [])) = continue $ modifyText s (handleChar c)
eventHandler s (VtyEvent (V.EvKey V.KUp [])) = continue $ modifyText s moveUp
eventHandler s (VtyEvent (V.EvKey V.KDown [])) = continue $ modifyText s moveDown
eventHandler s (VtyEvent (V.EvKey V.KLeft [])) = continue $ modifyText s moveLeft
eventHandler s (VtyEvent (V.EvKey V.KLeft [V.MShift])) = continue $ modifyText s selectLeft
eventHandler s (VtyEvent (V.EvKey V.KRight [])) = continue $ modifyText s moveRight
eventHandler s (VtyEvent (V.EvKey V.KRight [V.MShift])) = continue $ modifyText s selectRight
eventHandler s (VtyEvent (V.EvKey V.KEsc [])) = halt s
eventHandler s (VtyEvent (V.EvResize rows cols)) = continue $ resize s rows cols
eventHandler s _ = continue s

modifyText :: State -> (Cursor StyleChar -> Cursor StyleChar) -> State
modifyText s f = s {text = mapUnselected (\sc -> sc {style = Nothing}) $ mapSelected (\sc -> sc {style = Just tiltOn}) $ f $ text s}

handleChar :: Char -> Cursor StyleChar -> Cursor StyleChar
handleChar ch c = insert c (StyleChar ch Nothing)

resize :: State -> Int -> Int -> State
resize s rows cols = s {terminalSize = (rows, cols)}
