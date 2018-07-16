import           Application        as App
import           System.Environment
import qualified System.Exit        as E

main :: IO ()
main = getArgs >>= parse

parse ["-h"]        = usage >> exit
parse ["-help"]     = usage >> exit
parse ["-v"]        = version >> exit
parse ["--version"] = version >> exit
parse [filePath]    = App.start filePath
parse _             = usage >> die

usage = putStrLn "Usage: htxt <file_path>"

version = putStrLn "htxt 1.0"

exit = E.exitWith E.ExitSuccess

die = E.exitWith (E.ExitFailure 1)
