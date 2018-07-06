module Widget.SearchBar
  ( SearchBar(..)
  , renderSearchBar
  , handleSearchEvent
  , copy
  , cut
  , paste
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
handleSearchEvent (B.VtyEvent ev) s =
  case ev of
    V.EvKey V.KBS [] -> search (updateSearchBar (applyEdit deleteLeft (searchBar s)) (unsearch s))
    V.EvKey (V.KChar c) [] ->
      search (updateSearchBar (applyEdit (handleInsert c) (searchBar s)) (unsearch s))
    -- Commands
    V.EvKey V.KEnter [] -> moveToNextOccurrence s
    -- Movement
    V.EvKey V.KLeft [] -> updateSearchBar (applyEdit moveLeft (searchBar s)) s
    V.EvKey V.KRight [] -> updateSearchBar (applyEdit moveRight (searchBar s)) s
    -- Selection
    V.EvKey V.KLeft [V.MShift] -> updateSearchBar (applyEdit selectLeft (searchBar s)) s
    V.EvKey V.KRight [V.MShift] -> updateSearchBar (applyEdit selectRight (searchBar s)) s
    -- Other
    _ -> s
handleSearchEvent _ s = s

handleInsert :: Char -> C.Cursor StyleChar -> C.Cursor StyleChar
handleInsert c =
  case c of
    '\t' -> twice (insert (charWnoAttrs ' '))
    '\n' -> id
    c    -> insert (charWnoAttrs c)
  where
    twice f = f . f

applyEdit :: (C.Cursor StyleChar -> C.Cursor StyleChar) -> SearchBar -> SearchBar
applyEdit f sb = sb {query = (f . query) sb}

updateSearchBar :: SearchBar -> State -> State
updateSearchBar sb s = s {searchBar = sb}

search :: State -> State
search (State sb e f) = State newSB newE f
  where
    newSB = sb {currentOccurrences = positions}
    newE = e {contents = (moveToPosition p searched)}
    old = (head . getLines . query) sb
    new = map (\(StyleChar c (Attrs sel _)) -> StyleChar c (Attrs sel True)) old
    p = (getCurrentPosition . contents) e
    (searched, positions) = searchAndReplace old new (contents e)

moveToNextOccurrence :: State -> State
moveToNextOccurrence (State (SearchBar sbn sbc (p:ps)) e f) = State newSB newE f
  where
    newSB = SearchBar sbn sbc (ps ++ [p])
    newE = e {contents = foldl (\h c -> selectRight h) (moveToPosition p $ moveLeft $ contents e) query}
    query = head $ getLines sbc
moveToNextOccurrence s = s

unsearch :: State -> State
unsearch state = state {editor = editor'}
  where
    editor' = (editor state) {contents = unsearchCursor}
    unsearchCursor =
      foldr (\position h -> unsearchOneOccurrence q $ moveToPosition position h) cursor ps
    q = (head . getLines . query . searchBar) state
    ps = (currentOccurrences . searchBar) state
    cursor = (contents . editor) state
    unsearchOneOccurrence q c = foldl (\h ch -> insert ch $ deleteRight h) c $ map S.styleCharWnoAttrs q

copy :: SearchBar -> String
copy = toString . head . getSelectedLines . query

cut :: SearchBar -> (SearchBar, String)
cut sb = (sb {query = (deleteLeft . query) sb}, copy sb)

paste :: String -> SearchBar -> SearchBar
paste s sb = (foldl (\h x -> applyEdit (handleInsert x) h) sb firstLineOfS)
  where
    firstLineOfS = takeWhile (\x -> x /= '\n') s
