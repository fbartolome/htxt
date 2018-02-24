module State
  ( State (..)
  , StyleChar (..)
  , Mode (..)
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

data Mode = Insert
          | Search
          deriving (Show, Eq)

data State = State
  { text         :: Cursor StyleChar
  , undoText     :: Seq (Cursor StyleChar)
  , redoText     :: [Cursor StyleChar]
  , filename     :: Maybe String
  , terminalSize :: (Int, Int)
  , mode         :: Mode
  }

-- TODO en terminalSize buscar el tamaño de la terminal
empty :: State
empty = State {text = Cursor.empty, filename = Nothing, terminalSize = (30, 30),
 undoText = Empty, redoText = [], mode = Insert}

getFilename :: State -> String
getFilename (State {filename = Nothing})  = "*Unsaved file*"
getFilename (State {filename = (Just n)}) = n

-- Undo/Redo

undoLimit :: Int
undoLimit = 50

undo :: State -> State
undo (State t (u:<|us) rs fn s m) = State u us (t:rs) fn s m
undo s                          = s

redo :: State -> State
redo (State t us (r:rs) fn s m) = State r (t<|us) rs fn s m
redo s                        = s

pushUndo :: State -> State
pushUndo (State t us rs fn s m)
  | Data.Sequence.length us == undoLimit = State t (updateUndos us) [] fn s m
  | otherwise = State t (t<|us) [] fn s m where
  updateUndos (undos:|>u) = t<|undos
