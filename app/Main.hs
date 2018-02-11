import Control.Monad
import qualified Brick as B
import Brick.BChan (newBChan, writeBChan)
import qualified Graphics.Vty as V

import Screen
import Handler
import Drawer

main = do
    let s = emptyScreenZipper
    chan <- newBChan 10
    void $ B.customMain (V.mkVty V.defaultConfig) (Just chan) app s

-- TODO: ver bien cuales son los atributos default para la App
app = B.App { B.appDraw = drawUI
            , B.appChooseCursor = B.neverShowCursor 
            , B.appHandleEvent = eventHandler
            , B.appStartEvent = return
            , B.appAttrMap = const theMap
            }

theMap :: B.AttrMap
theMap = B.attrMap V.defAttr []


