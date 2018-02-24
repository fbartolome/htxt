module Application
  ( UIResource (..)
  , UIEvent (..)
  ) where

data UIResource = MainContent
                | EditorContent
                | EditorViewpoint
                | EditorCursor
                deriving (Ord, Show, Eq)

-- data UIEvent = ()
--                deriving (Ord, Show, Eq)
type UIEvent = ()
