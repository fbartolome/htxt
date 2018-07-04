module State where

import qualified Brick             as B

import           Data.File         as F
import           Data.Styled.StyleChar
import qualified Widget.Editor     as E
import           Widget.UIResource as UI

data Focus
  = Editor
  | TaskBar

data State = State
               --taskBar :: TaskBar
  { editor :: E.Editor
  , focus  :: Focus
  }

newState :: F.File -> [[StyleChar]] -> State
newState f tx = State {editor = E.makeEditor EditorContent f tx, focus = Editor}
