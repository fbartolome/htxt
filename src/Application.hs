module Application
  ( start
  ) where

import qualified Brick             as B
import           Brick.BChan       as B
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List.Split
import qualified Graphics.Vty      as V
import           System.IO

import           Data.File         as F
import           Data.Cursor
import           Data.Styled.Style
import           Data.Styled.StyleChar
import           State             as S
import           Widget.Editor     as E
import           Widget.UIResource as UI

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
          initState = S.newState (F.File file file) $ map (map (\c -> StyleChar c Nothing )) $ splitOn "\n" text
          -- TODO: tener en cuenta los tabs
      eventChan <- B.newBChan 10
      void $ B.customMain (V.mkVty V.defaultConfig) (Just eventChan) app initState
    [] ->
      putStrLn noArguments
    _ ->
      putStrLn tooManyArguments

renderApp :: State -> [B.Widget UI.UIResource]
renderApp s = [(E.renderEditor . editor) s True]

onStart :: State -> B.EventM UI.UIResource State
onStart = return

handleAppEvent ::
     State -> B.BrickEvent UI.UIResource UIEvent -> B.EventM UI.UIResource (B.Next State)
handleAppEvent s (B.VtyEvent (V.EvKey V.KEsc [])) = B.halt s
handleAppEvent s (B.VtyEvent (V.EvKey (V.KChar 's') [V.MCtrl])) = liftIO (save s) >>= B.continue
handleAppEvent s e = B.continue (s {editor = E.handleEditorEvent e (S.editor s)})

noArguments :: String
noArguments = "No arguments"

tooManyArguments :: String
tooManyArguments = "Too many arguments"

save :: State -> IO State
save s = do
  liftIO $ writeFile (F.filePath $ E.file $ S.editor s) $ foldr (\l h -> (map char l) ++ "\n" ++ h) "" $ getLines cursor
  return s
  where
    cursor = contents $ editor s
