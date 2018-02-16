module State
  ( State (..),
    StyleChar (..),
    State.empty,
    getFilename
  ) where

import Brick

import           Cursor

data StyleChar = StyleChar
  { char  :: Char,
    style :: Maybe AttrName
  }

data State = State
  { text     :: Cursor StyleChar,
    filename :: Maybe String
  }

empty :: State
empty = State {text = Cursor.empty, filename = Nothing}

getFilename :: State -> String
getFilename (State _ Nothing)  = "*Unsaved file*"
getFilename (State _ (Just n)) = n
