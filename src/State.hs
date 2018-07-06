module State
  ( State(..)
  , Focus(..)
  , Editor(..)
  , SearchBar(..)
  , newState
  ) where

import qualified Brick                 as B
import qualified Data.Sequence         as Seq

import           Data.Cursor           as C
import           Data.File             as F
import qualified Data.Styled.Style     as S
import           Data.Styled.StyleChar
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
  { searchBar = makeSearchBar SearchBarContent
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
  }

makeEditor :: UI.UIResource -> F.File -> [[StyleChar]] -> Editor
makeEditor n f tx =
  Editor
  { editorName = n
  , file = f
  , contents = C.newCursor tx (\(StyleChar c (Attrs sel sea)) -> StyleChar c (Attrs True sea)) (\(StyleChar c (Attrs sel sea)) -> StyleChar c (Attrs False sea))
  , size = (30, 30) -- TODO: Sacar de algun lado
  , undoLimit = 50
  , undoContents = Seq.Empty
  , redoContents = []
  }

data SearchBar = SearchBar
  { resourceName       :: UI.UIResource
  , query              :: C.Cursor StyleChar
  , currentOccurrences :: [C.Position]
  }

makeSearchBar :: UI.UIResource -> SearchBar
makeSearchBar n = SearchBar {resourceName = n, query = C.empty, currentOccurrences = []}
