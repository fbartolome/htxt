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
import           Data.Styled.StyleChar as S
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
  B.hBox ((B.str "Search: ") : (foldr (\sc h -> (S.renderChar sc) : h) [B.str " "] scs))

handleSearchEvent :: B.BrickEvent UI.UIResource e -> State -> State
handleSearchEvent (B.VtyEvent ev) state =
  case ev of
    V.EvKey V.KBS [] -> search $ (applyEdit deleteLeft) $ unsearch state
    V.EvKey (V.KChar '\t') [] -> state -- TODO: cambiara dos espacios
    V.EvKey (V.KChar c) [] -> search $ (applyEdit (insert (charWnoAttrs c))) $ unsearch state
    -- Commands
    V.EvKey V.KEnter [] -> moveToNextOccurrence state
    -- Movement
    V.EvKey V.KLeft [] -> applyEdit moveLeft state
    V.EvKey V.KRight [] -> applyEdit moveRight state
    -- Selection
    V.EvKey V.KLeft [V.MShift] -> applyEdit selectLeft state
    V.EvKey V.KRight [V.MShift] -> applyEdit selectRight state
    -- Other
    _ -> state
handleSearchEvent _ state = state

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
    new = map (\(StyleChar c (Attrs sel _)) -> StyleChar c (Attrs sel True)) old
    p = (getCurrentPosition . contents) e
    (searched, positions) = searchAndReplace old new (contents e)
    
moveToNextOccurrence :: State -> State
moveToNextOccurrence (State (SearchBar sbn sbc (p:ps)) e f) = State newSB newE f
  where
    newSB = SearchBar sbn sbc (ps ++ [p])
    newE = e {contents = foldl (\h c -> selectRight h) (moveToPosition (contents e) p) query}
    query = head $ getLines sbc
moveToNextOccurrence s = s

unsearch :: State -> State
unsearch state = state {editor = editor'}
  where
    editor' = (editor state) {contents = unsearchCursor}
    unsearchCursor = foldr (\position h -> unsearchOneOccurrence q $ moveToPosition h position) cursor ps
    q = (head . getLines . query . searchBar) state
    ps = (currentOccurrences . searchBar) state
    cursor = (contents . editor) state
    unsearchOneOccurrence q c = foldl (\h ch -> insert ch $ deleteRight h) c q
