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

newState :: F.File -> [[StyleChar]] -> (Int,Int) -> State
newState f tx size =
  State
  { searchBar = makeSearchBar SearchBarContent [[]]
  , editor = makeEditor EditorContent f tx size
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

makeEditor :: UI.UIResource -> F.File -> [[StyleChar]] -> (Int,Int) -> Editor
makeEditor n f tx s =
  Editor
  { editorName = n
  , file = f
  , contents = C.newCursor tx (SC.setSelection True) (SC.setSelection False)
  , size = s
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
  , query = C.newCursor tx (SC.setSelection True) (SC.setSelection False)
  , currentOccurrences = []
  , replaceContents = C.newCursor [] (SC.setSelection True) (SC.setSelection False)
  , searchBarFocus = OnSearch
  }
