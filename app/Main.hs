import qualified Brick         as B
import           Brick.BChan   (newBChan)
import           Control.Monad
import qualified Graphics.Vty  as V

import           Application
import           Cursor
import           Drawer
import           Handler
import           State         as S
import           Style

main :: IO ()
main = do
  let initState = S.empty
  eventChan <- newBChan 10
  void $ B.customMain (V.mkVty V.defaultConfig) (Just eventChan) app initState

app :: B.App State UIEvent UIResource
app = B.App
      { B.appDraw = drawUI
      , B.appChooseCursor = B.showFirstCursor
      , B.appHandleEvent = eventHandler
      , B.appStartEvent = return
      , B.appAttrMap = const theMap
      }
