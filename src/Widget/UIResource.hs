module Widget.UIResource
  ( UIResource(..)
  , UIEvent(..)
  ) where

data UIResource
  = MainContent
  | SearchBarContent
  | EditorContent
  | EditorViewpoint
  | EditorCursor
  deriving (Ord, Show, Eq)

type UIEvent = ()
