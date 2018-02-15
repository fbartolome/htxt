import Control.Monad
import Brick
import Brick.BChan (newBChan, writeBChan)
import qualified Graphics.Vty as V

import TextZipper
import State
import Handler
import Drawer
import Style

main = do
    let screenState = newScreenState
    chan <- newBChan 10
    void $ customMain (V.mkVty V.defaultConfig) (Just chan) app screenState

-- TODO: ver bien cuales son los atributos default para la App
app = App { appDraw = drawUI
            , appChooseCursor = neverShowCursor 
            , appHandleEvent = eventHandler
            , appStartEvent = return
            , appAttrMap = const theMap
            }





