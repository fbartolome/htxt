module Handler
(   Name,
    Tick,
    eventHandler
) where

import qualified Brick as B
import qualified Graphics.Vty as V

import Text
import State

type Name = ()
data Tick = Tick

eventHandler :: State -> B.BrickEvent Name Tick -> B.EventM Name (B.Next State)
eventHandler s (B.VtyEvent (V.EvKey V.KBS [])) = B.continue $ modifyText s delete
eventHandler s (B.VtyEvent (V.EvKey V.KEnter [])) = B.continue $ modifyText s newLine
eventHandler s (B.VtyEvent (V.EvKey (V.KChar c) [])) = B.continue $ modifyText s (handleChar c)
eventHandler s (B.VtyEvent (V.EvKey V.KUp [])) = B.continue $ modifyText s moveUp
eventHandler s (B.VtyEvent (V.EvKey V.KDown [])) = B.continue $ modifyText s moveDown
eventHandler s (B.VtyEvent (V.EvKey V.KLeft [])) = B.continue $ modifyText s moveLeft
eventHandler s (B.VtyEvent (V.EvKey V.KRight [])) = B.continue $ modifyText s moveRight
eventHandler s (B.VtyEvent (V.EvKey V.KEsc [])) = B.halt s
eventHandler s _ = B.continue s

modifyText :: State -> (TextZipper -> TextZipper) -> State
modifyText s f = s {text = f $ text s}

handleChar :: Char -> TextZipper -> TextZipper
handleChar c sz = newChar c sz