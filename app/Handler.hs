module Handler
(   Name,
    Tick,
    eventHandler
) where

import qualified Brick as B
import qualified Graphics.Vty as V

import Screen

type Name = ()
data Tick = Tick

eventHandler :: ScreenZipper -> B.BrickEvent Name Tick -> B.EventM Name (B.Next ScreenZipper)
eventHandler s (B.VtyEvent (V.EvKey V.KBS [])) = B.continue $ delete s
eventHandler s (B.VtyEvent (V.EvKey (V.KChar c) [])) = B.continue $ handleChar s c
eventHandler s (B.VtyEvent (V.EvKey V.KEnter [])) = B.continue $ newLine s
eventHandler s (B.VtyEvent (V.EvKey V.KUp [])) = B.continue $ moveUp s
eventHandler s (B.VtyEvent (V.EvKey V.KDown [])) = B.continue $ moveDown s
eventHandler s (B.VtyEvent (V.EvKey V.KLeft [])) = B.continue $ moveLeft s
eventHandler s (B.VtyEvent (V.EvKey V.KRight [])) = B.continue $ moveRight s
eventHandler s (B.VtyEvent (V.EvKey V.KEsc [])) = B.halt s
eventHandler s _ = B.continue s

handleChar :: ScreenZipper -> Char -> ScreenZipper
handleChar sz c = newChar c sz