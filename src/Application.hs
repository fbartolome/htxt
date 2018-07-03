module Application
  ( start
  ) where

import qualified Brick             as B
import           Brick.BChan       as B
import           Control.Monad
import qualified Graphics.Vty      as V

import           Data.File         as F
import           Data.Styled.Style
import           State             as S
import           Widget.Editor     as E
import           Widget.UIResource as UI

start :: IO ()
start = do
  let app =
        B.App
        { B.appDraw = renderApp
        , B.appStartEvent = onStart
        , B.appHandleEvent = handleAppEvent
        , B.appChooseCursor = B.showFirstCursor
        , B.appAttrMap = const theMap
        }
      initState = S.empty (F.File "asd" "asd")
  eventChan <- B.newBChan 10
  void $ B.customMain (V.mkVty V.defaultConfig) (Just eventChan) app initState

renderApp :: State -> [B.Widget UI.UIResource]
renderApp s = [(E.renderEditor . editor) s True]

onStart :: State -> B.EventM UI.UIResource State
onStart = return

handleAppEvent ::
     State -> B.BrickEvent UI.UIResource UIEvent -> B.EventM UI.UIResource (B.Next State)
handleAppEvent s (B.VtyEvent (V.EvKey V.KEsc [])) = B.halt s
handleAppEvent s e = B.continue (s {editor = E.handleEditorEvent e (S.editor s)})
