module Widget.Editor
  ( Editor(..)
  , renderEditor
  , handleEditorEvent
  , copy
  , cut
  , paste
  , resize
  , replaceAll
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

renderEditor :: Editor -> Bool -> B.Widget UI.UIResource
renderEditor e f =
  let cursorLocation = B.Location (cursorPosition e)
  in B.borderWithLabel (B.str $ " " ++ fileNameWithSave e ++ " ") $
     B.viewport (editorName e) B.Vertical $
     (if f
        then B.showCursor (editorName e) cursorLocation
        else id) $
     B.visibleRegion cursorLocation (1, 1) $
     renderContents $ adaptContents (size e) ((getLines . contents) e)
     where
       fileNameWithSave e
        | saved e == 0 = ((F.fileName) . file) e
        | otherwise = "* " ++ ((F.fileName) . file) e ++ " *"

renderContents :: [[StyleChar]] -> B.Widget UI.UIResource
renderContents scs = B.vBox $ map (\x -> renderLine x) scs

renderLine :: [StyleChar] -> B.Widget UI.UIResource
renderLine scs = B.hBox (foldr (\sc h -> (S.renderChar sc) : h) [B.str " "] scs)

adaptContents :: (Int, Int) -> [[StyleChar]] -> [[StyleChar]]
adaptContents _ [[]] = addLineIndicators [[charWnoAttrs ' ']]
adaptContents (r, _) s = foldr (\line h -> addLineIndicators (addEmptyLine (chunksOf (r - 4) line)) ++ h) [] s
  where
    addEmptyLine [] = [[charWnoAttrs ' ']]
    addEmptyLine ls
      | (length . last) ls == (r - 4) = ls ++ [[charWnoAttrs ' ']]
      | otherwise = ls

addLineIndicators :: [[StyleChar]] -> [[StyleChar]]
addLineIndicators (sc:scs) = (lineIndicator ++ sc):(map (\l -> notLineIndicator ++ l) scs)
  where
    lineIndicator = S.stringToStyleChars (S.Attrs False False True) "> "
    notLineIndicator = map (\sc -> sc {char = ' '}) lineIndicator
addLineIndicators []       = []


cursorPosition :: Editor -> (Int, Int)
cursorPosition e =
  sum (2, length upAdapted + selAddY) (additionalLength (leftListLength + selAddX) xLimit)
  where
    upAdapted = adaptContents (size e) ((up . contents) e)
    leftListLength = (length . left . contents) e
    firstListLength = (length . firstLine . fromJust . selection . contents) e
    (selAddX, selAddY) =
      case (selection . contents) e of
        Just (SL ss C.Right) -> (length ss, 0)
        Just (ML sus sls sds C.Right) ->
          ( length sds - leftListLength
          , (length . (adaptContents (size e))) sls +
            snd (additionalLength (leftListLength + firstListLength) xLimit) +
            1)
        _ -> (0, 0)
    xLimit = ((fst . size) e) - 4
    sum (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

additionalLength :: Int -> Int -> (Int, Int)
additionalLength length limit = (length - limit * remainder, remainder)
  where
    remainder = length `quot` limit

-- TODO: Add missing cases
handleEditorEvent :: B.BrickEvent UI.UIResource e -> Editor -> Editor
handleEditorEvent (B.VtyEvent ev) =
  case ev of
    V.EvKey V.KBS [] -> (applyEdit deleteLeft) . pushUndo
    V.EvKey V.KEnter [] -> (applyEdit handleNewLine) . pushUndo
    V.EvKey (V.KChar c) [] -> (applyEdit $ handleInsert c) . pushUndo
    V.EvKey (V.KBackTab) [] ->
      ((\f -> f . f) $ applyEdit $ removeAtLineStart $ charWnoAttrs ' ') . pushUndo
    -- Movement
    V.EvKey V.KLeft [] -> applyEdit moveLeft
    V.EvKey V.KRight [] -> applyEdit moveRight
    V.EvKey V.KDown [] -> handleMoveDown
    V.EvKey V.KUp [] -> handleMoveUp
    V.EvKey (V.KChar 'l') [V.MCtrl] -> applyEdit moveToLineEnd
    V.EvKey (V.KChar 'k') [V.MCtrl] -> applyEdit moveToLineStart
    V.EvKey (V.KChar 'e') [V.MCtrl] -> applyEdit moveToScreenStart
    V.EvKey (V.KChar 'd') [V.MCtrl] -> applyEdit moveToScreenEnd
    V.EvKey (V.KChar 'p') [V.MCtrl] -> applyEdit $ moveUntilNotSpace . moveUntilSpace . moveRight
      where
        space = S.charWnoAttrs ' '
        moveUntilSpace = moveRightUntil (space ==) True
        moveUntilNotSpace = moveRightUntil (not . (space  ==)) False
    V.EvKey (V.KChar 'o') [V.MCtrl] -> applyEdit $ moveUntilSpace . moveUntilNotSpace . moveLeft
      where
        space = S.charWnoAttrs ' '
        moveUntilSpace = moveLeftUntil (space ==) True
        moveUntilNotSpace = moveLeftUntil (not . (space  ==)) False
    -- Selection
    V.EvKey V.KLeft [V.MShift] -> applyEdit selectLeft
    V.EvKey V.KRight [V.MShift] -> applyEdit selectRight
    V.EvKey V.KDown [V.MShift] -> applyEdit selectDown
    V.EvKey V.KUp [V.MShift] -> applyEdit selectUp
    V.EvKey (V.KChar 'r') [V.MCtrl] -> applyEdit selectToLineStart
    V.EvKey (V.KChar 'u') [V.MCtrl] -> applyEdit selectToLineEnd
    V.EvKey (V.KChar 'a') [V.MCtrl] -> applyEdit selectAll
    -- Move content
    V.EvKey (V.KChar 't')  [V.MCtrl] -> applyEdit moveLinesWithSelectionUp
    V.EvKey (V.KChar 'g')  [V.MCtrl] -> applyEdit moveLinesWithSelectionDown
    -- Undo/Redo
    V.EvKey (V.KChar 'z') [V.MCtrl] -> undo
    V.EvKey (V.KChar 'y') [V.MCtrl] -> redo
    -- Default
    _ -> id
handleEditorEvent _ = id

handleInsert :: Char -> C.Cursor StyleChar -> C.Cursor StyleChar
handleInsert c cursor =
  case c of
    '\t' ->
      case selection cursor of
        Nothing -> twice (insert (charWnoAttrs ' ')) cursor
        Just s  -> twice (insertAtLineStart (charWnoAttrs ' ')) cursor
    '(' ->
      ((insertBeforeSelection (charWnoAttrs '(')) . (insertAfterSelection (charWnoAttrs ')')))
        cursor
    '[' ->
      ((insertBeforeSelection (charWnoAttrs '[')) . (insertAfterSelection (charWnoAttrs ']')))
        cursor
    '{' ->
      ((insertBeforeSelection (charWnoAttrs '{')) . (insertAfterSelection (charWnoAttrs '}')))
        cursor
    '\n' -> insertLine cursor
    c -> insert (charWnoAttrs c) cursor
  where
    twice f = f . f

handleNewLine :: C.Cursor StyleChar -> C.Cursor StyleChar
handleNewLine c =
  case selection c of
    Nothing -> foldl (\h sc -> (insert sc) h) (insertLine c) (indentation (reverse $ left c) False)
    Just a -> handleNewLine $ deleteLeft c
  where
    items = "-*+#>"
    space = charWnoAttrs ' '
    indentation [] _ = []
    indentation (sc:scs) False
      | sc == space = space : (indentation scs False)
      | (char sc) `elem` items = (charWnoAttrs (char sc)) : (indentation scs True)
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
    terminalLength = ((fst $ size e) - 4)
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
    terminalLength = ((fst $ size e) - 4)
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
applyEdit f e = e {contents = (f . contents) e}

resize :: (Int, Int) -> Editor -> Editor
resize s e = e {size = s}

--
-- Undo/Redo
--
undo :: Editor -> Editor
undo (Editor e f c s ul (u Seq.:<| us) rs saved) = Editor e f u s ul us (c : rs) (saved - 1)
undo s                                     = s

redo :: Editor -> Editor
redo (Editor e f c s ul us (r:rs) saved) = Editor e f r s ul (c Seq.<| us) rs (saved + 1)
redo s                             = s

pushUndo :: Editor -> Editor
pushUndo (Editor e f c s ul us rs saved)
  | Seq.length us == ul = Editor e f c s ul (updateUndos us) [] $ newSaved saved
  | otherwise = Editor e f c s ul (c Seq.<| us) []  $ newSaved saved
  where
    updateUndos (us Seq.:|> u) = c Seq.<| us
    newSaved n
      | n < 0 = ul + 1
      | otherwise = n + 1

copy :: Editor -> String
copy e = L.intercalate "\n" (map toString ((getSelectedLines . contents) e))

cut :: Editor -> (Editor, String)
cut e = (e {contents = (deleteLeft . contents) e}, copy e)

paste :: String -> Editor -> Editor
paste s e = (foldl (\h x -> applyEdit (handleInsert x) h) (pushUndo e) s)

replaceAll :: [StyleChar] -> [StyleChar] -> Editor -> Editor
replaceAll o n e = (pushUndo e) {contents = fst (searchAndReplace o n (contents e))}
