module State
  ( State (..)
  , StyleChar (..)
  , State.empty
  , getFilename
  ) where

import           Brick

import           Cursor

data StyleChar = StyleChar
  { char  :: Char
  , style :: Maybe AttrName
  }

data State = State
  { text         :: Cursor StyleChar
  , filename     :: Maybe String
  , terminalSize :: (Int, Int)
  }

-- TODO en terminalSize buscar el tamaño de la terminal
empty :: State
empty = State {text = Cursor.empty, filename = Nothing, terminalSize = (30, 30)}

getFilename :: State -> String
getFilename (State _ Nothing _)  = "*Unsaved file*"
getFilename (State _ (Just n) _) = n
