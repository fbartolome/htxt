module State
  ( State (..)
  , StyleChar (..)
  , State.empty
  , getFilename
  , undo
  , redo
  ) where

import           Brick

import           Cursor

data StyleChar = StyleChar
  { char  :: Char
  , style :: Maybe AttrName
  }

data State = State
  { text         :: Cursor StyleChar
  , undoText     :: [Cursor StyleChar]
  , redoText     :: [Cursor StyleChar]
  , filename     :: Maybe String
  , terminalSize :: (Int, Int)
  }

-- TODO en terminalSize buscar el tamaño de la terminal
empty :: State
empty = State {text = Cursor.empty, filename = Nothing, terminalSize = (30, 30),
 undoText = [], redoText = []}

getFilename :: State -> String
getFilename (State {filename = Nothing})  = "*Unsaved file*"
getFilename (State {filename = (Just n)}) = n

undo :: State -> State
undo (State t (u:us) rs fn s) = State u us (t:rs) fn s
undo s                        = s

redo :: State -> State
redo (State t us (r:rs) fn s) = State r (t:us) rs fn s
redo s                        = s
