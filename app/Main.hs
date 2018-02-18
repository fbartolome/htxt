import qualified Brick         as B
import           Brick.BChan   (newBChan, writeBChan)
import           Control.Monad
import qualified Graphics.Vty  as V

import           Cursor
import           Drawer
import           Handler
import           State         as S
import           Style

main = do
    let ss = S.empty
    chan <- newBChan 10
    void $ B.customMain (V.mkVty V.defaultConfig) (Just chan) app ss

-- TODO: ver bien cuales son los atributos default para la App
app = B.App { B.appDraw = drawUI
            , B.appChooseCursor = B.showFirstCursor
            , B.appHandleEvent = eventHandler
            , B.appStartEvent = return
            , B.appAttrMap = const theMap
            }
