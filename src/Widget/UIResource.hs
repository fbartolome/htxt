module Widget.UIResource
  ( UIResource(..)
  , UIEvent(..)
  ) where

data UIResource
  = MainContent
  | EditorContent
  | EditorViewpoint
  | EditorCursor
  deriving (Ord, Show, Eq)

type UIEvent = ()
