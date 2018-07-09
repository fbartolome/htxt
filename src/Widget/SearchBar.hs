module Widget.SearchBar
  ( SearchBar(..)
  , renderSearchBar
  , onShow
  , onHide
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
import           Data.Styled.StyleChar as SC
import           State
import qualified Widget.Editor         as E
import           Widget.UIResource     as UI

renderSearchBar :: SearchBar -> Bool -> B.Widget UI.UIResource
renderSearchBar t f =
  B.vLimit 2 $
  B.vBox
    [ B.hBox
        [ B.str "Search: "
        , B.viewport SearchBarSearchViewpoint B.Horizontal $
          toggleSearchCursor $
          B.visibleRegion searchCL (1, 1) $ renderContents ((getLines . query) t)
        ]
    , B.hBox
        [ B.str "Replace: "
        , B.viewport SearchBarReplaceViewpoint B.Horizontal $
          toggleReplaceCursor $
          B.visibleRegion replaceCL (1, 1) $ renderContents ((getLines . replaceContents) t)
        ]
    ]
  where
    searchCL = B.Location ((swap . getCurrentPosition . query) t)
    toggleSearchCursor =
      (if f && (searchBarFocus t) == OnSearch
         then B.showCursor (resourceName t) searchCL
         else id)
    replaceCL = B.Location ((swap . getCurrentPosition . replaceContents) t)
    toggleReplaceCursor =
      (if f && (searchBarFocus t) == OnReplace
         then B.showCursor (resourceName t) replaceCL
         else id)

renderContents :: [[StyleChar]] -> B.Widget UI.UIResource
renderContents scs = B.vBox $ map (\x -> renderLine x) scs

renderLine :: [StyleChar] -> B.Widget UI.UIResource
renderLine scs = B.hBox (foldr (\sc h -> (SC.renderChar sc) : h) [B.str " "] scs)

onShow :: State -> State
onShow (State sb e f) =
  case (selection . contents) e of
    Just (SL l C.Left) -> search $ State (newSB l) unselectedE f
    Just (SL l C.Right) -> search $ State (newSB (reverse l)) unselectedE f
    Just (ML fl _ _ C.Left) -> search $ State (newSB fl) unselectedE f
    Just (ML fl _ _ C.Right) -> search $ State (newSB (reverse fl)) unselectedE f
    _ -> search $ (State sb e f)
  where
    unselectedE = e {contents = (moveLeft . contents) e}
    sbToLineEnd sb = sb {query = (moveToLineEnd . query) sb}
    newSB l = sbToLineEnd (makeSearchBar SearchBarContent [map (SC.setSelection False) l])

onHide :: State -> State
onHide (State (SearchBar rn q [] rc sbf) e f) = (State (SearchBar rn q [] rc sbf) e f)
onHide s = unsearched {editor = newEditor}
  where
    unsearched = unsearch s
    p = (last . currentOccurrences . searchBar) s
    searched = (head . getLines . query . searchBar) s
    newEditor = (editor unsearched) {contents = foldr (\c h -> selectRight h) (moveToPosition p (contents $ editor unsearched)) searched}

handleSearchEvent :: B.BrickEvent UI.UIResource e -> State -> State
handleSearchEvent (B.VtyEvent ev) s =
  case ev of
    V.EvKey V.KBS [] -> deleteHandler
    V.EvKey (V.KChar '\t') [] -> toggleFocus s
    V.EvKey (V.KChar c) [] -> insertHandler c
    -- Commands
    V.EvKey V.KEnter [] -> enterHandler
    -- Movement
    V.EvKey V.KLeft [] -> updateSearchBar (applyEdit moveLeft (searchBar s)) s
    V.EvKey V.KRight [] -> updateSearchBar (applyEdit moveRight (searchBar s)) s
    -- Selection
    V.EvKey V.KLeft [V.MShift] -> updateSearchBar (applyEdit selectLeft (searchBar s)) s
    V.EvKey V.KRight [V.MShift] -> updateSearchBar (applyEdit selectRight (searchBar s)) s
    -- Other
    _ -> s
  where
    deleteHandler =
      case (searchBarFocus . searchBar) s of
        OnSearch -> search (updateSearchBar (applyEdit deleteLeft (searchBar s)) (unsearch s))
        OnReplace -> updateSearchBar (applyEdit deleteLeft (searchBar s)) s
    insertHandler c =
      case (searchBarFocus . searchBar) s of
        OnSearch -> search (updateSearchBar (applyEdit (handleInsert c) (searchBar s)) (unsearch s))
        OnReplace -> updateSearchBar (applyEdit (handleInsert c) (searchBar s)) s
    enterHandler =
      case (searchBarFocus . searchBar) s of
        OnSearch  -> moveToNextOccurrence s
        OnReplace -> search (replaceAllOccurrences (unsearch s))
handleSearchEvent _ s = s

toggleFocus :: State -> State
toggleFocus (State sb e f) = State (sb {searchBarFocus = newF}) e f
  where
    newF =
      case searchBarFocus sb of
        OnSearch  -> OnReplace
        OnReplace -> OnSearch

handleInsert :: Char -> C.Cursor StyleChar -> C.Cursor StyleChar
handleInsert c =
  case c of
    '\t' -> twice (insert (charWnoAttrs ' '))
    '\n' -> id
    c    -> insert (charWnoAttrs c)
  where
    twice f = f . f

applyEdit :: (C.Cursor StyleChar -> C.Cursor StyleChar) -> SearchBar -> SearchBar
applyEdit f sb =
  case (searchBarFocus sb) of
    OnSearch  -> sb {query = (f . query) sb}
    OnReplace -> sb {replaceContents = (f . replaceContents) sb}

updateSearchBar :: SearchBar -> State -> State
updateSearchBar sb s = s {searchBar = sb}

search :: State -> State
search (State sb e f) = moveToNextOccurrence (State newSB newE f)
  where
    newSB = sb {currentOccurrences = positions}
    newE = e {contents = (moveToPosition p searched)}
    old = (head . getLines . query) sb
    new = map (SC.setSearch True) old
    p = (getCurrentPosition . contents) e
    (searched, positions) = searchAndReplace old new (contents e)

moveToNextOccurrence :: State -> State
moveToNextOccurrence (State (SearchBar sbn sbc (p:ps) sbr sbf) e f) = State newSB newE f
  where
    newSB = SearchBar sbn sbc (ps ++ [p]) sbr sbf
    newE =
      e {contents = foldl (\h c -> selectRight h) (moveToPosition p $ moveLeft $ contents e) query}
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
    unsearchOneOccurrence q c =
      foldl (\h ch -> insert ch $ deleteRight h) c $ map SC.styleCharWnoAttrs q

replaceAllOccurrences :: State -> State
replaceAllOccurrences s
  | [] <- (currentOccurrences sb) = s
  | otherwise = s {editor = E.replaceAll old new (editor s)}
  where
    sb = (searchBar s)
    old = (head . getLines . query) sb
    new = (head . getLines . replaceContents) sb

copy :: SearchBar -> String
copy = toString . head . getSelectedLines . query

cut :: SearchBar -> (SearchBar, String)
cut sb = (sb {query = (deleteLeft . query) sb}, copy sb)

paste :: String -> SearchBar -> SearchBar
paste s sb = (foldl (\h x -> applyEdit (handleInsert x) h) sb firstLineOfS)
  where
    firstLineOfS = takeWhile (\x -> x /= '\n') s
