import           Application as App
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  App.start args
