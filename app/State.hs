module State
  ( State (..)
  , StyleChar (..)
  , State.empty
  , getFilename
  , undo
  , redo
  , pushUndo
  ) where

import           Brick
import           Data.Sequence

import           Cursor

data StyleChar = StyleChar
  { char  :: Char
  , style :: Maybe AttrName
  }

data State = State
  { text         :: Cursor StyleChar
  , undoText     :: Seq (Cursor StyleChar)
  , redoText     :: [Cursor StyleChar]
  , filename     :: Maybe String
  , terminalSize :: (Int, Int)
  }

-- TODO en terminalSize buscar el tamaño de la terminal
empty :: State
empty = State {text = Cursor.empty, filename = Nothing, terminalSize = (30, 30),
 undoText = Empty, redoText = []}

getFilename :: State -> String
getFilename (State {filename = Nothing})  = "*Unsaved file*"
getFilename (State {filename = (Just n)}) = n

-- Undo/Redo

undoLimit :: Int
undoLimit = 50

undo :: State -> State
undo (State t (u:<|us) rs fn s) = (State u us (t:rs) fn s)
undo s                          = s

redo :: State -> State
redo (State t us (r:rs) fn s) = State r (t<|us) rs fn s
redo s                        = s

pushUndo :: State -> State
pushUndo (State t us rs fn s)
  | Data.Sequence.length us == undoLimit = State t (updateUndos us) [] fn s
  | otherwise = State t (t<|us) [] fn s where
  updateUndos (undos:|>u) = t<|undos
