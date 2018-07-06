module Application
  ( start
  ) where

import qualified Brick                  as B
import           Brick.BChan            as B
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List.Split
import qualified Graphics.Vty           as V
import qualified System.Hclip           as HC
import           System.IO

import           Data.Cursor
import           Data.File              as F
import           Data.Styled.Style
import           Data.Styled.StyleChar
import           State                  as S
import qualified Widget.Editor          as E
import qualified Widget.SearchBar       as SB
import           Widget.UIResource      as UI

start :: [String] -> IO ()
start args = do
  case args of
    [file] -> do
      handle <- openFile file ReadWriteMode
      text <- hGetContents handle
      let app =
            B.App
            { B.appDraw = renderApp
            , B.appStartEvent = onStart
            , B.appHandleEvent = handleAppEvent
            , B.appChooseCursor = B.showFirstCursor
            , B.appAttrMap = const theMap
            }
          initState =
            S.newState (F.makeFile file) $ map (map (\c -> charWnoAttrs c)) $ splitOn "\n" text
          -- TODO: tener en cuenta los tabs
      eventChan <- B.newBChan 10
      void $ B.customMain (V.mkVty V.defaultConfig) (Just eventChan) app initState
    [] -> putStrLn noArguments
    _ -> putStrLn tooManyArguments

renderApp :: State -> [B.Widget UI.UIResource]
renderApp (State sb e f) =
  case f of
    OnEditor -> [B.vBox [(E.renderEditor e) True]]
    OnSearchBar -> [B.vBox [(E.renderEditor e) False, (SB.renderSearchBar sb) True]]

onStart :: State -> B.EventM UI.UIResource State
onStart = return

handleAppEvent ::
     State -> B.BrickEvent UI.UIResource UIEvent -> B.EventM UI.UIResource (B.Next State)
handleAppEvent s (B.VtyEvent (V.EvKey V.KEsc [])) = B.halt s
handleAppEvent s (B.VtyEvent (V.EvKey (V.KChar 's') [V.MCtrl])) = liftIO (save s) >>= B.continue
handleAppEvent s (B.VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = liftIO (copy s) >>= B.continue
handleAppEvent s (B.VtyEvent (V.EvKey (V.KChar 'x') [V.MCtrl])) = liftIO (cut s) >>= B.continue
handleAppEvent s (B.VtyEvent (V.EvKey (V.KChar 'v') [V.MCtrl])) = liftIO (paste s) >>= B.continue
handleAppEvent s (B.VtyEvent (V.EvKey (V.KChar 'f') [V.MCtrl])) = B.continue (s {focus = f})
  where
    f =
      case focus s of
        OnEditor    -> OnSearchBar
        OnSearchBar -> OnEditor
-- Resize
handleAppEvent s (B.VtyEvent (V.EvResize r c)) = B.continue (s {editor = resizedEditor})
  where resizedEditor = E.resize (r,c) $ editor s
-- Other
handleAppEvent s e =
  case focus s of
    OnEditor -> B.continue (s {editor = E.handleEditorEvent e (S.editor s)})
    OnSearchBar -> B.continue (SB.handleSearchEvent e s)

-- TODO: Pasar a Main.hs
noArguments :: String
noArguments = "No arguments"

tooManyArguments :: String
tooManyArguments = "Too many arguments"

save :: State -> IO State
save s = do
  writeFile (F.filePath $ E.file $ S.editor s) $
    foldr (\l h -> (map char l) ++ "\n" ++ h) "" $ getLines cursor
  return s
  where
    cursor = E.contents $ editor s

copy :: State -> IO State
copy s = do
  HC.setClipboard c
  return s
  where
    c =
      case focus s of
        OnEditor    -> (E.copy . editor) s
        OnSearchBar -> (SB.copy . searchBar) s

cut :: State -> IO State
cut (State sb e OnEditor) = do
  HC.setClipboard c
  return (State sb newE OnEditor)
  where
    (newE, c) = E.cut e
cut (State sb e OnSearchBar) = do
  HC.setClipboard c
  return (State newSB e OnSearchBar)
  where
    (newSB, c) = SB.cut sb

paste :: State -> IO State
paste s = do
  c <- HC.getClipboard
  return (newS c)
  where
    newS c =
      case focus s of
        OnEditor    -> s {editor = E.paste c (editor s)}
        OnSearchBar -> s {searchBar = SB.paste c (searchBar s)}
