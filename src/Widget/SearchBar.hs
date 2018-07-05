module Widget.SearchBar
  ( SearchBar(..)
  , renderSearchBar
  , handleSearchEvent
  ) where

import qualified Brick                 as B
import           Brick.Widgets.Border  as B
import qualified Data.List             as L
import           Data.List.Split
import           Data.Maybe
import qualified Data.Sequence         as Seq
import           Data.Tuple
import qualified Graphics.Vty          as V

import           Data.Cursor           as C
import           Data.File             as F
import qualified Data.Styled.Style     as S
import           Data.Styled.StyleChar
import           State
import           Widget.UIResource     as UI

renderSearchBar :: SearchBar -> Bool -> B.Widget UI.UIResource
renderSearchBar t f =
  let cursorLocation = B.Location ((\(x, y) -> (x + 8, y)) ((swap . getCurrentPosition . query) t))
  in B.vLimit 1 $
     B.viewport (resourceName t) B.Horizontal $
     (if f
        then B.showCursor (resourceName t) cursorLocation
        else id) $
     B.visibleRegion cursorLocation (1, 1) $ renderContents ((getLines . query) t)

renderContents :: [[StyleChar]] -> B.Widget UI.UIResource
renderContents scs = B.vBox $ map (\x -> renderLine x) scs

renderLine :: [StyleChar] -> B.Widget UI.UIResource
renderLine scs =
  B.hBox ((B.str "Search: ") : (foldr (\sc h -> (renderChar sc) : h) [B.str " "] scs))

renderChar :: StyleChar -> B.Widget UI.UIResource
renderChar sc
  | style sc == Nothing = B.str [char sc]
  | otherwise = B.withAttr ((fromJust . style) sc) $ B.str [char sc]

handleSearchEvent :: B.BrickEvent UI.UIResource e -> State -> State
handleSearchEvent (B.VtyEvent ev) =
  case ev of
    V.EvKey V.KBS [] -> search . (applyEdit deleteLeft)
    V.EvKey (V.KChar '\t') [] -> id
    V.EvKey (V.KChar c) [] -> search . (applyEdit (insert (StyleChar c Nothing)))
    -- Commands
    V.EvKey V.KEnter [] -> moveToNextOccurrence
    -- Movement
    V.EvKey V.KLeft [] -> applyEdit moveLeft
    V.EvKey V.KRight [] -> applyEdit moveRight
    -- Selection
    V.EvKey V.KLeft [V.MShift] -> applyEdit selectLeft
    V.EvKey V.KRight [V.MShift] -> applyEdit selectRight
    -- Other
    _ -> id
handleSearchEvent _ = id

-- TODO: Ver q onda colores cuando no tienen foco
-- TODO: Remove style (add it's logic to Cursor)
applyEdit :: (C.Cursor StyleChar -> C.Cursor StyleChar) -> State -> State
applyEdit func (State sb e f) = State newSB e f
  where
    newSB = sb {query = (func . query) sb}

search :: State -> State
search (State sb e f) = State newSB newE f
  where
    newSB = sb {currentOccurrences = positions}
    newE = e {contents = (moveToPosition searched p)}
    old = (head . getLines . query) sb
    new = map (\(StyleChar c _) -> StyleChar c (Just S.search)) old
    p = (getCurrentPosition . contents) e
    (searched, positions) = searchAndReplace old new (contents e)

moveToNextOccurrence :: State -> State
moveToNextOccurrence (State (SearchBar sbn sbc (p:ps)) e f) = State newSB newE f
  where
    newSB = SearchBar sbn sbc (ps ++ [p])
    newE = e {contents = (moveToPosition (contents e) p)}
moveToNextOccurrence s = s
