module State where

import qualified Brick             as B

import           Data.File         as F
import           Widget.Editor     as E
import           Widget.UIResource as UI

data Focus
  = Editor
  | TaskBar

data State = State
               --taskBar :: TaskBar
  { editor :: E.Editor
  , focus  :: Focus
  }

empty :: F.File -> State
empty f = State {editor = E.makeEditor EditorContent f, focus = Editor}
