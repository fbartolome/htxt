import Screen
import System.IO
import Control.Monad
import Data.Char
import System.Posix.Signals

main = do
    hSetEcho stdin False
    installHandler keyboardSignal (Catch handler) Nothing
    forever $ do
        return ()


    -- hSetEcho stdin False
    -- hSetBuffering stdin NoBuffering
    -- c <- getChar
    -- print $ ord c

-- main = do 
--     hSetEcho stdin False
--     hSetBuffering stdin NoBuffering
--     inputLoop

-- inputLoop :: IO ()
-- inputLoop = do i <- getChar
--                mapM_ putChar $ takeWhile ((/= 27) . ord) i

-- ifReadyDo :: Handle -> IO a -> IO (Maybe a)
-- ifReadyDo hnd x = hReady hnd >>= f
--    where f True = x >>= return . Just
--          f _    = return Nothing

-- putChar' (Just c) = putChar c 
-- putChar' Nothing = return () 

handler = do
    putChar 'r'
    return ()