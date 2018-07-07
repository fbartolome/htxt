module Widget.UIResource
  ( UIResource(..)
  , UIEvent(..)
  ) where

data UIResource
  = MainContent
  | SearchBarContent
  | SearchBarSearchViewpoint
  | SearchBarReplaceViewpoint
  | EditorContent
  | EditorViewpoint
  | EditorCursor
  deriving (Ord, Show, Eq)

type UIEvent = ()
