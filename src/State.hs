module State
  ( State(..)
  , Focus(..)
  , Editor(..)
  , SearchBarFocus(..)
  , SearchBar(..)
  , newState
  , makeEditor
  , makeSearchBar
  ) where

import qualified Brick                 as B
import qualified Data.Sequence         as Seq

import           Data.Cursor           as C
import           Data.File             as F
import           Data.Styled.StyleChar as SC
import           Widget.UIResource     as UI

data Focus
  = OnEditor
  | OnSearchBar
  deriving (Eq)

data State = State
  { searchBar :: SearchBar
  , editor    :: Editor
  , focus     :: Focus
  }

newState :: F.File -> [[StyleChar]] -> State
newState f tx =
  State
  { searchBar = makeSearchBar SearchBarContent [[]]
  , editor = makeEditor EditorContent f tx
  , focus = OnEditor
  }

data Editor = Editor
  { editorName   :: UI.UIResource
  , file         :: F.File
  , contents     :: C.Cursor StyleChar
  , size         :: (Int, Int) -- (r, c)
  , undoLimit    :: Int
  , undoContents :: Seq.Seq (C.Cursor StyleChar)
  , redoContents :: [C.Cursor StyleChar]
  , saved        :: Int
  }

makeEditor :: UI.UIResource -> F.File -> [[StyleChar]] -> Editor
makeEditor n f tx =
  Editor
  { editorName = n
  , file = f
  , contents = C.newCursor tx SC.selectionOn SC.selectionOff
  , size = (30, 30) -- TODO: Sacar de algun lado
  , undoLimit = 50
  , undoContents = Seq.Empty
  , redoContents = []
  , saved = 0
  }

data SearchBarFocus
  = OnSearch
  | OnReplace
  deriving (Eq)

data SearchBar = SearchBar
  { resourceName       :: UI.UIResource
  , query              :: C.Cursor StyleChar
  , currentOccurrences :: [C.Position]
  , replaceContents    :: C.Cursor StyleChar
  , searchBarFocus     :: SearchBarFocus
  }

makeSearchBar :: UI.UIResource -> [[StyleChar]] -> SearchBar
makeSearchBar n tx =
  SearchBar
  { resourceName = n
  , query = C.newCursor tx SC.selectionOn SC.selectionOff
  , currentOccurrences = []
  , replaceContents = C.newCursor [] SC.selectionOn SC.selectionOff
  , searchBarFocus = OnSearch
  }
