import Control.Monad
import Brick
import Brick.BChan (newBChan, writeBChan)
import Control.Concurrent (threadDelay, forkIO)
import qualified Graphics.Vty as V

import TextZipper
import State
import Handler
import Drawer
import Style

tiltSpeed = 500000

main = do
    chan <- newBChan 10
    forkIO $ forever $ do
        writeBChan chan Handler.Tick
        threadDelay tiltSpeed
    let screenState = newScreenState
    void $ customMain (V.mkVty V.defaultConfig) (Just chan) app screenState

-- TODO: ver bien cuales son los atributos default para la App
app = App { appDraw = drawUI
            , appChooseCursor = neverShowCursor 
            , appHandleEvent = eventHandler
            , appStartEvent = return
            , appAttrMap = const theMap
            }





