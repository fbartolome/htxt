module State
(   State (..),
    newScreenState,
    getName
) where

import TextZipper

data State = State 
   { text :: TextZipper,
     name :: Maybe String,
     cursorTilt :: Bool
   }

noName = "This file has no name"

newScreenState :: State
newScreenState = State {text = emptyTextZipper, name = Nothing, cursorTilt = False}

getName :: State -> String
getName (State _ Nothing _) = noName
getName (State _ (Just n) _) = n