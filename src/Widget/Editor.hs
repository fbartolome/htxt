module Widget.Editor
  ( Editor(..)
  , renderEditor
  , handleEditorEvent
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

renderEditor :: Editor -> Bool -> B.Widget UI.UIResource
renderEditor e f =
  let cursorLocation = B.Location ((swap . getCurrentPosition . contents) e)
  in B.borderWithLabel (B.str $ " " ++ (((F.fileName) . file) e) ++ " ") $
     B.viewport (editorName e) B.Vertical $
     (if f
        then B.showCursor (editorName e) cursorLocation
        else id) $
     B.visibleRegion cursorLocation (1, 1) $
     renderContents $ adaptContents (size e) ((getLines . contents) e)

renderContents :: [[StyleChar]] -> B.Widget UI.UIResource
renderContents scs = B.vBox $ map (\x -> renderLine x) scs

renderLine :: [StyleChar] -> B.Widget UI.UIResource
renderLine scs = B.hBox (foldr (\sc h -> (renderChar sc) : h) [B.str " "] scs)

renderChar :: StyleChar -> B.Widget UI.UIResource
renderChar sc
  | style sc == Nothing = B.str [char sc]
  | otherwise = B.withAttr ((fromJust . style) sc) $ B.str [char sc]

adaptContents :: (Int, Int) -> [[StyleChar]] -> [[StyleChar]]
adaptContents _ [[]] = [[StyleChar ' ' Nothing]]
adaptContents (r, _) s = foldr (\line h -> (changeEmptyLine (chunksOf (r - 2) line)) ++ h) [] s
  where
    changeEmptyLine [] = [[StyleChar ' ' Nothing]]
    changeEmptyLine l  = l

-- TODO: Add missing cases
handleEditorEvent :: B.BrickEvent UI.UIResource e -> Editor -> Editor
handleEditorEvent (B.VtyEvent ev) =
  case ev of
    V.EvKey V.KBS [] -> (applyEdit deleteLeft) . pushUndo
    V.EvKey V.KEnter [] -> (applyEdit handleNewLine) . pushUndo
    V.EvKey (V.KChar c) [] -> (applyEdit $ handleInsert c) . pushUndo
    V.EvKey (V.KBackTab) [] ->
      ((\f -> f . f) $ applyEdit $ removeAtLineStart $ StyleChar ' ' Nothing) . pushUndo
    -- Movement
    V.EvKey V.KLeft [] -> applyEdit moveLeft
    V.EvKey V.KRight [] -> applyEdit moveRight
    V.EvKey V.KDown [] -> handleMoveDown
    V.EvKey V.KUp [] -> handleMoveUp
    -- Selection
    V.EvKey V.KLeft [V.MShift] -> applyEdit selectLeft
    V.EvKey V.KRight [V.MShift] -> applyEdit selectRight
    -- Move content
    V.EvKey V.KDown [V.MShift] -> applyEdit moveLinesWithSelectionDown
    V.EvKey V.KUp [V.MShift] -> applyEdit moveLinesWithSelectionUp
    -- Undo/Redo
    V.EvKey (V.KChar 'z') [V.MCtrl] -> undo
    V.EvKey (V.KChar 'y') [V.MCtrl] -> redo
    -- Other
    V.EvResize r c -> resize (r, c)
    _ -> id
handleEditorEvent _ = id

handleInsert :: Char -> C.Cursor StyleChar -> C.Cursor StyleChar
handleInsert c cursor =
  case c of
    '\t' ->
      case selection cursor of
        Nothing -> twice (insert (StyleChar ' ' Nothing)) cursor
        Just s  -> twice (insertAtLineStart (StyleChar ' ' Nothing)) cursor
    c -> insert (StyleChar c Nothing) cursor
  where
    twice f = f . f

handleNewLine :: C.Cursor StyleChar -> C.Cursor StyleChar
handleNewLine c =
  case selection c of
    Nothing -> foldl (\h sc -> (insert sc) h) (insertLine c) (indentation (reverse $ left c) False)
    Just a -> handleNewLine $ deleteLeft c
  where
    items = ['-', '*', '+', '#', '>']
    space = StyleChar ' ' Nothing
    indentation [] _ = []
    indentation (sc:scs) False
      | sc == space = space : (indentation scs False)
      | (char sc) `elem` items = (StyleChar (char sc) Nothing) : (indentation scs True)
      | otherwise = []
    indentation (sc:scs) True
      | sc == space = space : (indentation scs True)
      | otherwise = []

handleMoveDown :: Editor -> Editor
handleMoveDown e
  | isJust selected =
    let f =
          case selected of
            Just (SL _ C.Left)      -> moveLeft
            Just (ML _ _ _ C.Left)  -> moveLeft
            Just (SL _ C.Right)     -> moveRight
            Just (ML _ _ _ C.Right) -> moveRight
    in handleMoveDown $ applyEdit f e
  | rightOnCursor > terminalLength = foldr (\n h -> applyEdit moveRight h) e [1 .. terminalLength]
  | length (down $ contents e) == 0 = applyEdit moveToLineEnd e
  | rightOnCursor > rightOnTerminal = applyEdit moveToLineEnd e
  | otherwise =
    foldr
      (\n h -> applyEdit moveRight h)
      (applyEdit (moveDown . moveToLineStart) e)
      [1 .. movingCharsFromLeft]
  where
    terminalLength = ((fst $ size e) - 2)
    rightOnCursor = (length $ right $ contents e)
    leftOnTerminal = (length $ left $ contents e) `mod` terminalLength
    rightOnTerminal = terminalLength - leftOnTerminal
    movingCharsFromLeft = min leftOnTerminal (length $ head $ down $ contents e)
    selected = selection $ contents e

handleMoveUp :: Editor -> Editor
handleMoveUp e
  | isJust (selection $ contents e) =
    let f =
          case selected of
            Just (SL _ C.Left)      -> moveLeft
            Just (ML _ _ _ C.Left)  -> moveLeft
            Just (SL _ C.Right)     -> moveRight
            Just (ML _ _ _ C.Right) -> moveRight
    in handleMoveUp $ applyEdit f e
  | leftOnCursor > terminalLength = foldr (\n h -> applyEdit moveLeft h) e [1 .. terminalLength]
  | length (up $ contents e) == 0 = applyEdit moveToLineStart e
  | leftOnCursor > upLineTerminalLength = applyEdit (moveToLineEnd . moveUp) e
  | otherwise =
    foldr
      (\n h -> applyEdit moveRight h)
      (applyEdit (moveUp . moveToLineStart) e)
      [1 .. movingCharsFromLeft]
  where
    terminalLength = ((fst $ size e) - 2)
    leftOnCursor = (length $ left $ contents e)
    leftOnTerminal = (length $ left $ contents e) `mod` terminalLength
    upLineCursorLength = (length $ head $ up $ contents e)
    upLineTerminalLength = upLineCursorLength `mod` terminalLength
    movingCharsFromLeft =
      (upLineCursorLength `quot` terminalLength) * terminalLength + leftOnTerminal
    selected = selection $ contents e

-- TODO: Ver q onda colores cuando no tienen foco
-- TODO: Remove style (add it's logic to Cursor)
applyEdit :: (C.Cursor StyleChar -> C.Cursor StyleChar) -> Editor -> Editor
applyEdit f e =
  e
  { contents =
      mapUnselected (\sc -> sc {style = Nothing}) $
      mapSelected (\sc -> sc {style = Just S.tiltOn}) $ (f . contents) e
  }

resize :: (Int, Int) -> Editor -> Editor
resize s e = e {size = s}

--
-- Undo/Redo
--
undo :: Editor -> Editor
undo (Editor e f c s ul (u Seq.:<| us) rs) = Editor e f u s ul us (c : rs)
undo s                                     = s

redo :: Editor -> Editor
redo (Editor e f c s ul us (r:rs)) = Editor e f r s ul (c Seq.<| us) rs
redo s                             = s

pushUndo :: Editor -> Editor
pushUndo (Editor e f c s ul us rs)
  | Seq.length us == ul = Editor e f c s ul (updateUndos us) []
  | otherwise = Editor e f c s ul (c Seq.<| us) []
  where
    updateUndos (us Seq.:|> u) = c Seq.<| us
